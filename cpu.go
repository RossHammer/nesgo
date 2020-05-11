package nesgo

import (
	"fmt"
)

type CPU struct {
	Mapper Mapper
	RAM    []uint8

	PC          uint16
	A, X, Y, SP uint8

	Cycles    int
	ScanLine  int
	newCycles int

	C, Z, I, D, B, V, N bool
}

func New(rom *Rom) *CPU {
	return &CPU{
		PC:       0xC000,
		I:        true,
		SP:       0xFD,
		ScanLine: 241,
		Mapper:   rom.Mapper,
		RAM:      make([]uint8, 2*1024),
	}
}

func (c *CPU) ExecuteOnce() {
	code := c.readPC()
	p1 := c.peekPC()
	i, ok := instructions[code]
	if !ok {
		i = unknownInstruction
	}

	p1Func := func() uint8 {
		c.PC++
		return p1
	}
	p2Func := c.readPC

	tick := func() {
		c.newCycles++
	}
	addr := func() uint16 {
		return i.addrMode.getAddress(c, p1Func, p2Func, c.realReadAddress, tick)
	}

	value := func() uint8 {
		return i.addrMode.getValue(c, p1Func, p2Func, c.realReadAddress, tick)
	}

	switch a := i.action; a {
	case Jump:
		c.PC = addr()
	case LoadX:
		c.X = c.setFlagsFromLoad(value())
	case StoreX:
		c.writeAddress(addr(), c.X)
	case JumpSubroutine:
		addr := addr()
		upper, lower := decomposeBytes(c.PC - 1)
		c.pushStack(lower)
		c.pushStack(upper)
		c.PC = addr
		c.newCycles++
	case NoOperation:
	case SetCarry:
		c.C = true
	case BranchIfCarrySet:
		c.conditionalBranch(c.C, addr())
	case ClearCarry:
		c.C = false
	case BranchIfCarryClear:
		c.conditionalBranch(!c.C, addr())
	case LoadAccumulator:
		c.A = c.setFlagsFromLoad(value())
	case BranchIfEqual:
		c.conditionalBranch(c.Z, addr())
	case BranchIfNotEqual:
		c.conditionalBranch(!c.Z, addr())
	case StoreAccumulator:
		c.writeAddress(addr(), c.A)
	case BitTest:
		v := value()
		c.Z = c.A&v == 0
		c.V = ((v >> 6) & 0x1) == 1
		c.N = ((v >> 7) & 0x1) == 1
	case BranchIfOverflowSet:
		c.conditionalBranch(c.V, addr())
	case BranchIfOverflowClear:
		c.conditionalBranch(!c.V, addr())
	case BranchIfPositive:
		c.conditionalBranch(!c.N, addr())
	case ReturnSubroutine:
		addr := combineBytes(c.popStack(), c.popStack()) + 1
		c.PC = addr
		c.newCycles += 2
	case SetInteruptDisable:
		c.I = true
	case SetDecimal:
		c.D = true
	case PushProcessorStatus:
		c.pushStack(c.GetStatus(true))
	case PullAccumulator:
		c.A = c.setFlagsFromLoad(c.popStack())
		c.newCycles++
	case LogicalAnd:
		c.A = c.setFlagsFromLoad(c.A & value())
	case Compare:
		c.compare(c.A, value())
	case ClearDecimal:
		c.D = false
	case PushAccumulator:
		c.pushStack(c.A)
	case PullProcessorStatus:
		c.SetStatus(c.popStack())
		c.newCycles++
	case BranchIfMinus:
		c.conditionalBranch(c.N, addr())
	case LogicalInclusiveOr:
		c.A = c.setFlagsFromLoad(c.A | value())
	case ClearOverflow:
		c.V = false
	case ExclusiveOr:
		c.A = c.setFlagsFromLoad(c.A ^ value())
	case AddWithCarry:
		c.add(value())
	case LoadY:
		c.Y = c.setFlagsFromLoad(value())
	case CompareY:
		c.compare(c.Y, value())
	case CompareX:
		c.compare(c.X, value())
	case SubtractWithCarry:
		c.add(^value())
	case IncrementY:
		c.Y = c.setFlagsFromLoad(c.Y + 1)
	case IncrementX:
		c.X = c.setFlagsFromLoad(c.X + 1)
	case DecrementY:
		c.Y = c.setFlagsFromLoad(c.Y - 1)
	case DecrementX:
		c.X = c.setFlagsFromLoad(c.X - 1)
	case TransferAccumulatorToY:
		c.Y = c.setFlagsFromLoad(c.A)
	case TransferAccumulatorToX:
		c.X = c.setFlagsFromLoad(c.A)
	case TransferYToAccumulator:
		c.A = c.setFlagsFromLoad(c.Y)
	case TransferXToAccumulator:
		c.A = c.setFlagsFromLoad(c.X)
	case TransferXToStackPointer:
		c.X = c.setFlagsFromLoad(c.SP)
	case TransferStackPointerToX:
		c.SP = c.X
	case ReturnFromInterrupt:
		c.SetStatus(c.popStack())
		addr := combineBytes(c.popStack(), c.popStack())
		c.PC = addr
		c.newCycles++
	case LogicalShiftRight:
		v := value()
		c.A = c.setFlagsFromLoad(v >> 1)
		c.C = v&0x1 == 1
	case ArithmeticShitLeft:
		v := value()
		c.A = c.setFlagsFromLoad(v << 1)
		c.C = v>>7 == 1
	case RotateRight:
		v := value()
		cr := uint8(0)
		if c.C {
			cr = 0x80
		}
		c.A = c.setFlagsFromLoad(v>>1 | cr)
		c.C = v&0x1 == 1
	case RotateLeft:
		v := value()
		cr := uint8(0)
		if c.C {
			cr = 0x1
		}
		c.A = c.setFlagsFromLoad(v<<1 | cr)
		c.C = v>>7 == 1
	default:
		panic(fmt.Sprintf("Unknown action %d", a))
	}

	cycles := c.newCycles * 3
	c.Cycles = cycles % 341
	c.ScanLine = (242+cycles/341)%262 - 1
}

func (c *CPU) conditionalBranch(v bool, addr uint16) {
	if v {
		c.PC = addr
		c.newCycles++
	}
}

func (c *CPU) compare(p, v uint8) {
	c.C = p >= v
	c.setFlagsFromLoad(p - v)
}

func (c *CPU) add(v uint8) {
	a := c.A
	cr := uint16(0)
	if c.C {
		cr = 1
	}
	sum := uint16(a) + uint16(v) + cr
	c.setFlagsFromLoad(uint8(sum))
	c.C = sum > 0xFF
	c.V = ^(a^v)&(a^uint8(sum))&0x80 != 0
	c.A = uint8(sum)
}

func (c *CPU) debugReadAddress(addr uint16) uint8 { return c.readAddress(addr, true) }
func (c *CPU) realReadAddress(addr uint16) uint8  { return c.readAddress(addr, false) }

func (c *CPU) readAddress(addr uint16, debug bool) uint8 {
	if !debug {
		c.newCycles++
	}
	switch {
	case addr < 0x0800:
		return c.RAM[addr]
	case addr >= 0x4020:
		return c.Mapper.ReadAddress(addr)
	}

	panic(fmt.Sprintf("Invalid read addr %04X", addr))
}

func (c *CPU) writeAddress(addr uint16, value uint8) {
	c.newCycles++
	switch {
	case addr < 0x0800:
		c.RAM[addr] = value
	default:
		panic(fmt.Sprintf("Invalid write addr %04X", addr))
	}
}

func (c *CPU) getInstruction(debug bool) *instruction {
	mem := c.readAddress(c.PC, debug)

	i, ok := instructions[mem]
	if !ok {
		return unknownInstruction
	}

	return i
}

func (c *CPU) pushStack(v uint8) {
	// fmt.Printf("push %04X %02X %02X\n", c.PC, c.SP, v)
	c.writeAddress(0x100|uint16(c.SP), v)
	c.SP--
}

func (c *CPU) popStack() uint8 {
	c.SP++
	v := c.readAddress(0x100|uint16(c.SP), false)
	// fmt.Printf("pop  %04X %02X %02X\n", c.PC, c.SP, v)
	return v
}

func (c *CPU) setFlagsFromLoad(v uint8) uint8 {
	c.Z = v == 0
	c.N = v>>7 != 0
	return v
}

func (c *CPU) readPC() uint8 {
	v := c.readAddress(c.PC, false)
	c.PC++
	return v
}
func (c *CPU) peekPC() uint8 {
	return c.readAddress(c.PC, false)
}

func (c *CPU) GetStatus(b bool) uint8 {
	p := uint8(0x20)
	p = setFlag(p, c.C, 0x1)
	p = setFlag(p, c.Z, 0x2)
	p = setFlag(p, c.I, 0x4)
	p = setFlag(p, c.D, 0x8)
	p = setFlag(p, b, 0x10)
	p = setFlag(p, c.V, 0x40)
	p = setFlag(p, c.N, 0x80)
	return p
}

func (c *CPU) SetStatus(flags uint8) {
	c.C = (flags & 0x1) == 1
	c.Z = (flags >> 1 & 0x1) == 1
	c.I = (flags >> 2 & 0x1) == 1
	c.D = (flags >> 3 & 0x1) == 1
	c.V = (flags >> 6 & 0x1) == 1
	c.N = (flags >> 7 & 0x1) == 1
}

func setFlag(p uint8, v bool, b uint8) uint8 {
	if !v {
		return p
	}

	return p | b
}

func (c *CPU) Instruction() []uint8 {
	i := c.getInstruction(true)
	return i.bytes(c)
}

func (c *CPU) InstructionString() string {
	ins := c.getInstruction(true)
	return ins.Disassembly(c)
}
