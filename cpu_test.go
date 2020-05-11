package nesgo

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"path"
	"path/filepath"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

const (
	tmpDir = ".tmp"
	romURL = "https://github.com/christopherpow/nes-test-roms/raw/master/other/nestest.nes"
	logURL = "https://github.com/christopherpow/nes-test-roms/raw/master/other/nestest.log"
)

func TestCPUWithROM(t *testing.T) {
	romData, err := getFile(romURL)
	assert.NoError(t, err)

	output, err := getFile(logURL)
	assert.NoError(t, err)

	rom, err := LoadRom(romData)
	assert.NoError(t, err)

	cpu := New(rom)

	var prev string
	for i, l := range strings.Split(string(output), "\n") {
		state := printState(cpu)
		if !assert.Equalf(t, l, state, "Processor failed on line %d of log", i+1) {
			assert.FailNow(t, prev)
		}

		cpu.ExecuteOnce()
		prev = l
	}
}

func printState(cpu *CPU) string {
	insBytes := ""
	for _, b := range cpu.Instruction() {
		insBytes += fmt.Sprintf("%02X ", b)
	}

	return fmt.Sprintf("%04X  %-9s %-31s A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d SL:%d",
		cpu.PC, insBytes, cpu.InstructionString(),
		cpu.A, cpu.X, cpu.Y, cpu.GetStatus(false), cpu.SP, cpu.Cycles, cpu.ScanLine)
}

func getFile(location string) ([]byte, error) {
	if err := os.Mkdir(tmpDir, 0700); !os.IsExist(err) {
		return nil, err
	}

	parsed, err := url.Parse(location)
	if err != nil {
		return nil, err
	}

	_, tmpName := path.Split(parsed.Path)
	tmpFile := filepath.Join(tmpDir, tmpName)

	cont, err := ioutil.ReadFile(tmpFile)
	if !os.IsNotExist(err) {
		return cont, err
	}

	resp, err := http.Get(location)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("Bad status code %d", resp.StatusCode)
	}

	cont, err = ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	return cont, ioutil.WriteFile(tmpFile, cont, 0666)
}
