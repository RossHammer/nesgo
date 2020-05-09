package nesgo

import "fmt"

type Mapper interface {
	ReadAddress(addr uint16) uint8
}

func NewMapper(mapperType uint8, romData []uint8) (Mapper, error) {
	switch mapperType {
	case 0:
		return &FixedMapper{ROMData: romData}, nil
	default:
		return nil, fmt.Errorf("unsupported mapper %d", mapperType)
	}
}

type FixedMapper struct {
	ROMData []uint8
}

func (m *FixedMapper) ReadAddress(addr uint16) uint8 {
	switch {
	case addr >= 0xC000:
		return m.ROMData[addr-0xC000]
	case addr >= 0x8000:
		return m.ROMData[addr-0x6000]
	case addr >= 0x6000:
		panic(fmt.Sprintf("RAM %X", addr))
	}

	panic(addr)
}
