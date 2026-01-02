package jsonrpc

import "core:bufio"
import "core:fmt"
import "core:io"
import "core:strconv"
import "core:strings"

Stream :: struct {
	input:  io.Reader,
	output: io.Writer,
	reader: bufio.Reader,
}

init :: proc(input: io.Reader, output: io.Writer) -> Stream {
	s: Stream
	s.input = input
	s.output = output
	bufio.reader_init(&s.reader, s.input)
	return s
}

read :: proc(stream: ^Stream) -> ([]byte, io.Error) {
	content_len: int
	for {
		line, err := bufio.reader_read_string(&stream.reader, '\n')
		if err != nil {
			return nil, err
		}
		line = strings.trim_space(line)
		if len(line) == 0 {
			break
		}

		parts, _ := strings.split_n(line, ":", 2)
		if len(parts) == 2 {
			key := strings.trim_space(parts[0])
			value := strings.trim_space(parts[1])
			if key == "Content-Length" {
				parsed_ok: bool
				content_len, parsed_ok = strconv.parse_int(value, 10)
				if !parsed_ok {
					return nil, nil
				}
			}
		}
	}
	if content_len == 0 {
		return nil, nil
	}
	body := make([dynamic]byte, content_len)
	total_read := 0
	for total_read < content_len {
		actual_len, err := io.read_full(stream.input, body[total_read:])
		if err != nil {
			return body[:], err
		}
		total_read += actual_len
	}
	return body[:], nil
}

write :: proc(stream: ^Stream, data: []byte) -> io.Error {
	header := fmt.aprintf("Content-Length: %d\r\n\r\n", len(data))
	if _, err := io.write(stream.output, transmute([]byte)header); err != nil {
		return err
	}
	if _, err := io.write(stream.output, data); err != nil {
		return err
	}
	return nil
}
