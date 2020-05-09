package nesgo

import (
	"fmt"
)

type instruction struct {
	code     uint8
	addrMode AddressingMode
	action   Action
}

type AddressingMode uint8

const (
	UnknownMode AddressingMode = iota
	Absolute
	Immediate
	ZeroPage
	Implied
	Relative
)

func (a AddressingMode) Length() uint16 {
	switch a {
	case UnknownMode:
		return 0
	case Absolute:
		return 2
	case Immediate:
		return 1
	case ZeroPage:
		return 1
	case Implied:
		return 0
	case Relative:
		return 1
	default:
		panic(fmt.Sprintf("Unknown addressing mode %d", a))
	}
}

func (a AddressingMode) getAddress(cpu *CPU, pc uint16) uint16 {
	switch a {
	case Absolute:
		return combineBytes(cpu.readAddress(pc+1), cpu.readAddress(pc+2))
	case ZeroPage:
		return uint16(cpu.readAddress(pc + 1))
	case Relative:
		return 1 + a.Length() + pc + uint16(cpu.readAddress(pc+1))
	default:
		panic(fmt.Sprintf("Unknown addressing mode %d", a))
	}
}

func (a AddressingMode) getValue(cpu *CPU, pc uint16) uint8 {
	switch a {
	case Immediate:
		return cpu.readAddress(pc + 1)
	case ZeroPage:
		return cpu.readAddress(a.getAddress(cpu, pc))
	default:
		panic(fmt.Sprintf("Unknown addressing mode %d", a))
	}
}

func (a AddressingMode) formattedAddr(cpu *CPU) string {
	switch a {
	case UnknownMode:
		return "???"
	case Absolute:
		return fmt.Sprintf("$%04X", combineBytes(cpu.readAddress(cpu.PC+1), cpu.readAddress(cpu.PC+2)))
	case Immediate:
		return fmt.Sprintf("#$%02X", cpu.readAddress(cpu.PC+1))
	case ZeroPage:
		return fmt.Sprintf("$%02X", cpu.readAddress(cpu.PC+1))
	case Implied:
		return ""
	case Relative:
		return fmt.Sprintf("$%04X", 1+a.Length()+cpu.PC+uint16(cpu.readAddress(cpu.PC+1)))
	default:
		panic(fmt.Sprintf("Unknown addressing mode %d", a))
	}
}

type Action uint8

const (
	UnknownAction Action = iota
	Jump
	LoadX
	StoreX
	JumpSubroutine
	NoOperation
	SetCarry
	BranchIfCarrySet
	ClearCarry
	BranchIfCarryClear
	LoadAccumulator
	BranchIfEqual
	BranchIfNotEqual
	StoreAccumulator
	BitTest
	BranchIfOverflowSet
	BranchIfOverflowClear
	BranchIfPositive
	ReturnSubroutine
	SetInteruptDisable
	SetDecimal
	PushProcessorStatus
	PullAccumulator
	LogicalAnd
	Compare
	ClearDecimal
	PushAccumulator
	PullProcessorStatus
	BranchIfMinus
	LogicalInclusiveOr
	ClearOverflow
	ExclusiveOr
	AddWithCarry
	LoadY
	CompareY
	CompareX
	SubtractWithCarry
	IncrementY
	IncrementX
	DecrementY
	DecrementX
	TransferAccumulatorToY
	TransferAccumulatorToX
	TransferYToAccumulator
	TransferXToAccumulator
	TransferStackToX
)

func (a Action) symbol() string {
	switch a {
	case UnknownAction:
		return "UNK"
	case Jump:
		return "JMP"
	case LoadX:
		return "LDX"
	case StoreX:
		return "STX"
	case JumpSubroutine:
		return "JSR"
	case NoOperation:
		return "NOP"
	case SetCarry:
		return "SEC"
	case BranchIfCarrySet:
		return "BCS"
	case ClearCarry:
		return "CLC"
	case BranchIfCarryClear:
		return "BCC"
	case LoadAccumulator:
		return "LDA"
	case BranchIfEqual:
		return "BEQ"
	case BranchIfNotEqual:
		return "BNE"
	case StoreAccumulator:
		return "STA"
	case BitTest:
		return "BIT"
	case BranchIfOverflowSet:
		return "BVS"
	case BranchIfOverflowClear:
		return "BVC"
	case BranchIfPositive:
		return "BPL"
	case ReturnSubroutine:
		return "RTS"
	case SetInteruptDisable:
		return "SEI"
	case SetDecimal:
		return "SED"
	case PushProcessorStatus:
		return "PHP"
	case PullAccumulator:
		return "PLA"
	case LogicalAnd:
		return "AND"
	case Compare:
		return "CMP"
	case ClearDecimal:
		return "CLD"
	case PushAccumulator:
		return "PHA"
	case PullProcessorStatus:
		return "PLP"
	case BranchIfMinus:
		return "BMI"
	case LogicalInclusiveOr:
		return "ORA"
	case ClearOverflow:
		return "CLV"
	case ExclusiveOr:
		return "EOR"
	case AddWithCarry:
		return "ADC"
	case LoadY:
		return "LDY"
	case CompareY:
		return "CPY"
	case CompareX:
		return "CPX"
	case SubtractWithCarry:
		return "SBC"
	case IncrementY:
		return "INY"
	case IncrementX:
		return "INX"
	case DecrementY:
		return "DEY"
	case DecrementX:
		return "DEX"
	case TransferAccumulatorToY:
		return "TAY"
	case TransferAccumulatorToX:
		return "TAX"
	case TransferYToAccumulator:
		return "TYA"
	case TransferXToAccumulator:
		return "TXA"
	case TransferStackToX:
		return "TSX"
	default:
		panic(fmt.Sprintf("Unknown action %d", a))
	}
}

func (a Action) printSource() bool {
	switch a {
	case StoreX:
		return true
	case StoreAccumulator:
		return true
	case BitTest:
		return true
	}

	return false
}

var (
	unknownInstruction = &instruction{}
	instructions       map[uint8]*instruction
	instructionList    = []*instruction{
		{code: 0x08, action: PushProcessorStatus, addrMode: Implied},
		{code: 0x09, action: LogicalInclusiveOr, addrMode: Immediate},
		{code: 0x10, action: BranchIfPositive, addrMode: Relative},
		{code: 0x18, action: ClearCarry, addrMode: Implied},
		{code: 0x20, action: JumpSubroutine, addrMode: Absolute},
		{code: 0x24, action: BitTest, addrMode: ZeroPage},
		{code: 0x28, action: PullProcessorStatus, addrMode: Implied},
		{code: 0x29, action: LogicalAnd, addrMode: Immediate},
		{code: 0x30, action: BranchIfMinus, addrMode: Relative},
		{code: 0x48, action: PushAccumulator, addrMode: Implied},
		{code: 0x49, action: ExclusiveOr, addrMode: Immediate},
		{code: 0x4C, action: Jump, addrMode: Absolute},
		{code: 0x38, action: SetCarry, addrMode: Implied},
		{code: 0x50, action: BranchIfOverflowClear, addrMode: Relative},
		{code: 0x60, action: ReturnSubroutine, addrMode: Implied},
		{code: 0x68, action: PullAccumulator, addrMode: Implied},
		{code: 0x69, action: AddWithCarry, addrMode: Immediate},
		{code: 0x70, action: BranchIfOverflowSet, addrMode: Relative},
		{code: 0x78, action: SetInteruptDisable, addrMode: Implied},
		{code: 0x85, action: StoreAccumulator, addrMode: ZeroPage},
		{code: 0x86, action: StoreX, addrMode: ZeroPage},
		{code: 0x88, action: DecrementY, addrMode: Implied},
		{code: 0x8A, action: TransferXToAccumulator, addrMode: Implied},
		// {code: 0x8E, action: StoreX, addrMode: Absolute},
		{code: 0x90, action: BranchIfCarryClear, addrMode: Relative},
		{code: 0x98, action: TransferYToAccumulator, addrMode: Implied},
		{code: 0xA0, action: LoadY, addrMode: Immediate},
		{code: 0xA2, action: LoadX, addrMode: Immediate},
		{code: 0xA8, action: TransferAccumulatorToY, addrMode: Implied},
		{code: 0xA9, action: LoadAccumulator, addrMode: Immediate},
		{code: 0xAA, action: TransferAccumulatorToX, addrMode: Implied},
		{code: 0xB0, action: BranchIfCarrySet, addrMode: Relative},
		{code: 0xB8, action: ClearOverflow, addrMode: Implied},
		{code: 0xBA, action: TransferStackToX, addrMode: Implied},
		{code: 0xC0, action: CompareY, addrMode: Immediate},
		{code: 0xC8, action: IncrementY, addrMode: Implied},
		{code: 0xC9, action: Compare, addrMode: Immediate},
		{code: 0xCA, action: DecrementX, addrMode: Implied},
		{code: 0xD0, action: BranchIfNotEqual, addrMode: Relative},
		{code: 0xD8, action: ClearDecimal, addrMode: Implied},
		{code: 0xE0, action: CompareX, addrMode: Immediate},
		{code: 0xE8, action: IncrementX, addrMode: Implied},
		{code: 0xE9, action: SubtractWithCarry, addrMode: Immediate},
		{code: 0xEA, action: NoOperation, addrMode: Implied},
		{code: 0xF0, action: BranchIfEqual, addrMode: Relative},
		{code: 0xF8, action: SetDecimal, addrMode: Implied},
	}
)

func init() {
	instructions = make(map[uint8]*instruction, len(instructionList))

	for _, i := range instructionList {
		_, ok := instructions[i.code]
		if ok {
			panic("OMG")
		}

		instructions[i.code] = i
	}
}

func (i *instruction) Disassembly(cpu *CPU) string {
	dis := fmt.Sprintf("%s %s", i.action.symbol(), i.addrMode.formattedAddr(cpu))

	if i.action.printSource() {
		s := cpu.readAddress(i.addrMode.getAddress(cpu, cpu.PC))
		dis = fmt.Sprintf("%s = %02X", dis, s)
	}

	return dis
}

func (i *instruction) bytes(cpu *CPU) []uint8 {
	length := i.addrMode.Length() + 1
	r := make([]uint8, length)

	for i := uint16(0); i < length; i++ {
		r[i] = cpu.readAddress(cpu.PC + i)
	}

	return r
}

func (i *instruction) execute(cpu *CPU) uint {
	pc := cpu.PC
	cpu.PC += 1 + i.addrMode.Length()

	var cycles uint

	switch a := i.action; a {
	case Jump:
		addr := i.addrMode.getAddress(cpu, pc)
		cpu.PC = addr
		cycles += 3
	case LoadX:
		value := i.addrMode.getValue(cpu, pc)
		setFlagsFromLoad(cpu, value)
		cpu.X = value
		cycles += 2
	case StoreX:
		cpu.writeAddress(i.addrMode.getAddress(cpu, pc), cpu.X)
		cycles += 3
	case JumpSubroutine:
		addr := i.addrMode.getAddress(cpu, pc)
		upper, lower := decomposeBytes(cpu.PC)
		cpu.pushStack(lower)
		cpu.pushStack(upper)
		cpu.PC = addr
		cycles += 6
	case NoOperation:
		cycles += 2
	case SetCarry:
		cpu.C = true
		cycles += 2
	case BranchIfCarrySet:
		cycles += i.comparison(cpu, cpu.C, pc)
	case ClearCarry:
		cpu.C = false
		cycles += 2
	case BranchIfCarryClear:
		cycles += i.comparison(cpu, !cpu.C, pc)
	case LoadAccumulator:
		value := i.addrMode.getValue(cpu, pc)
		setFlagsFromLoad(cpu, value)
		cpu.A = value
		cycles += 2
	case BranchIfEqual:
		cycles += i.comparison(cpu, cpu.Z, pc)
	case BranchIfNotEqual:
		cycles += i.comparison(cpu, !cpu.Z, pc)
	case StoreAccumulator:
		cpu.writeAddress(i.addrMode.getAddress(cpu, pc), cpu.A)
		cycles += 3
	case BitTest:
		v := i.addrMode.getValue(cpu, pc)
		cpu.Z = cpu.A&v == 0
		cpu.V = ((v >> 6) & 0x1) == 1
		cpu.N = ((v >> 7) & 0x1) == 1
		cycles += 3
	case BranchIfOverflowSet:
		cycles += i.comparison(cpu, cpu.V, pc)
	case BranchIfOverflowClear:
		cycles += i.comparison(cpu, !cpu.V, pc)
	case BranchIfPositive:
		cycles += i.comparison(cpu, !cpu.N, pc)
	case ReturnSubroutine:
		addr := combineBytes(cpu.popStack(), cpu.popStack())
		cpu.PC = addr
		cycles += 6
	case SetInteruptDisable:
		cycles += 2
		cpu.I = true
	case SetDecimal:
		cycles += 2
		cpu.D = true
	case PushProcessorStatus:
		cycles += 3
		cpu.pushStack(cpu.GetStatus(true))
	case PullAccumulator:
		cycles += 4
		v := cpu.popStack()
		setFlagsFromLoad(cpu, v)
		cpu.A = v
	case LogicalAnd:
		v := cpu.A & i.addrMode.getValue(cpu, pc)
		setFlagsFromLoad(cpu, v)
		cpu.A = v
		cycles += 2
	case Compare:
		v := i.addrMode.getValue(cpu, pc)
		cpu.C = cpu.A >= v
		setFlagsFromLoad(cpu, cpu.A-v)
		cycles += 2
	case ClearDecimal:
		cycles += 2
		cpu.D = false
	case PushAccumulator:
		cpu.pushStack(cpu.A)
		cycles += 3
	case PullProcessorStatus:
		cpu.SetStatus(cpu.popStack())
		cycles += 4
	case BranchIfMinus:
		cycles += i.comparison(cpu, cpu.N, pc)
	case LogicalInclusiveOr:
		v := cpu.A | i.addrMode.getValue(cpu, pc)
		setFlagsFromLoad(cpu, v)
		cpu.A = v
		cycles += 2
	case ClearOverflow:
		cpu.V = false
		cycles += 2
	case ExclusiveOr:
		v := cpu.A ^ i.addrMode.getValue(cpu, pc)
		setFlagsFromLoad(cpu, v)
		cpu.A = v
		cycles += 2
	case AddWithCarry:
		v := i.addrMode.getValue(cpu, pc)
		a := cpu.A
		c := uint16(0)
		if cpu.C {
			c = 1
		}
		sum := uint16(a) + uint16(v) + c
		setFlagsFromLoad(cpu, uint8(sum))
		cpu.C = sum > 0xFF
		cpu.V = ^(a^v)&(a^uint8(sum))&0x80 != 0
		cpu.A = uint8(sum)
		cycles += 2
	case LoadY:
		value := i.addrMode.getValue(cpu, pc)
		setFlagsFromLoad(cpu, value)
		cpu.Y = value
		cycles += 2
	case CompareY:
		v := i.addrMode.getValue(cpu, pc)
		cpu.C = cpu.Y >= v
		setFlagsFromLoad(cpu, cpu.Y-v)
		cycles += 2
	case CompareX:
		v := i.addrMode.getValue(cpu, pc)
		cpu.C = cpu.X >= v
		setFlagsFromLoad(cpu, cpu.X-v)
		cycles += 2
	case SubtractWithCarry:
		v := ^i.addrMode.getValue(cpu, pc)
		a := cpu.A
		c := uint16(0)
		if cpu.C {
			c = 1
		}
		sum := uint16(a) + uint16(v) + c
		setFlagsFromLoad(cpu, uint8(sum))
		cpu.C = sum > 0xFF
		cpu.V = ^(a^v)&(a^uint8(sum))&0x80 != 0
		cpu.A = uint8(sum)
		cycles += 2
	case IncrementY:
		v := cpu.Y
		v++
		setFlagsFromLoad(cpu, v)
		cpu.Y = v
		cycles += 2
	case IncrementX:
		v := cpu.X
		v++
		setFlagsFromLoad(cpu, v)
		cpu.X = v
		cycles += 2
	case DecrementY:
		v := cpu.Y
		v--
		setFlagsFromLoad(cpu, v)
		cpu.Y = v
		cycles += 2
	case DecrementX:
		v := cpu.X
		v--
		setFlagsFromLoad(cpu, v)
		cpu.X = v
		cycles += 2
	case TransferAccumulatorToY:
		v := cpu.A
		setFlagsFromLoad(cpu, v)
		cpu.Y = v
		cycles += 2
	case TransferAccumulatorToX:
		v := cpu.A
		setFlagsFromLoad(cpu, v)
		cpu.X = v
		cycles += 2
	case TransferYToAccumulator:
		v := cpu.Y
		setFlagsFromLoad(cpu, v)
		cpu.A = v
		cycles += 2
	case TransferXToAccumulator:
		v := cpu.X
		setFlagsFromLoad(cpu, v)
		cpu.A = v
		cycles += 2
	case TransferStackToX:
		v := cpu.SP
		setFlagsFromLoad(cpu, v)
		cpu.X = v
		cycles += 2
	default:
		panic(fmt.Sprintf("Unknown action %d", a))
	}

	return cycles
}

func (i *instruction) comparison(cpu *CPU, flag bool, pc uint16) uint {
	cycles := uint(2)
	if flag {
		cpu.PC = i.addrMode.getAddress(cpu, pc)
		cycles++
	}

	return cycles
}

func setFlagsFromLoad(cpu *CPU, v uint8) {
	cpu.Z = v == 0
	cpu.N = v>>7 != 0
}

func combineBytes(a, b uint8) uint16 {
	return uint16(a) | (uint16(b) << 8)
}

func decomposeBytes(a uint16) (uint8, uint8) {
	return uint8(a), uint8(a >> 8)
}
