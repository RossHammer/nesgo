package nesgo

import "fmt"

type CPU struct {
	Cycles uint
	Mapper Mapper
	RAM    []uint8

	PC          uint16
	A, X, Y, SP uint8
	ScanLine    uint

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
	i := c.getInstruction()
	cycles := i.execute(c)
	c.Cycles += 3 * cycles
	if c.Cycles >= 341 {
		c.Cycles -= 341
		c.ScanLine++
	}
	// if c.ScanLine >= 256 {
	// 	c.ScanLine -= 256
	// }
}

func (c *CPU) readAddress(addr uint16) uint8 {
	switch {
	case addr < 0x0800:
		return c.RAM[addr]
	case addr >= 0x4020:
		return c.Mapper.ReadAddress(addr)
	}

	panic(fmt.Sprintf("Invalid read addr %04X", addr))
}

func (c *CPU) writeAddress(addr uint16, value uint8) {
	switch {
	case addr < 0x0800:
		c.RAM[addr] = value
	default:
		panic(fmt.Sprintf("Invalid write addr %04X", addr))
	}
}

func (c *CPU) getInstruction() *instruction {
	mem := c.readAddress(c.PC)

	i, ok := instructions[mem]
	if !ok {
		return unknownInstruction
	}

	return i
}

func (c *CPU) pushStack(v uint8) {
	c.writeAddress(uint16(c.SP), v)
	c.SP--
}

func (c *CPU) popStack() uint8 {
	c.SP++
	return c.readAddress(uint16(c.SP))
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

func (c *CPU) readPC() uint8 {
	c.PC++
	return c.readAddress(c.PC)
}

func (c *CPU) Instruction() []uint8 {
	i := c.getInstruction()
	return i.bytes(c)
}

func (c *CPU) InstructionString() string {
	ins := c.getInstruction()
	return ins.Disassembly(c)
}
