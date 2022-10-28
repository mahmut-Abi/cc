//go:build !go1.19

package cc

import "os/exec"

func command(name string, arg ...string) *exec.Cmd {
	cmd := exec.Command(name, arg...)
	return cmd
}
