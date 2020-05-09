package nesgo

import (
	"bytes"
	"errors"
)

type Rom struct {
	Mapper Mapper
}

type INES struct {
	Data []uint8
}

func LoadRom(data []byte) (*Rom, error) {
	// romData := make([]uint8, 1000)
	switch string(data[:4]) {
	case "NES\x1A": // iNES
		header := data[4:16]
		if !bytes.Equal(header, []byte{0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}) {
			panic("Unknown file")
		}

		romSize := 1 * 16384

		romData := make([]uint8, romSize)
		if copy(romData, data[16:]) != romSize {
			panic("Bad rom")
		}

		mapper, err := NewMapper(0, romData)
		if err != nil {
			return nil, err
		}

		return &Rom{
			Mapper: mapper,
		}, nil
	default:
		return nil, errors.New("unknown rom type")
	}
}
