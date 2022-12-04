// Copyright 2022 The CC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package cc // import "modernc.org/cc/v4"

import (
	"encoding/binary"
	"fmt"
)

var (
	byteOrders = map[string]binary.ByteOrder{
		"386":     binary.LittleEndian,
		"amd64":   binary.LittleEndian,
		"arm":     binary.LittleEndian,
		"arm64":   binary.LittleEndian,
		"ppc64le": binary.LittleEndian,
		"riscv64": binary.LittleEndian,
		"s390x":   binary.BigEndian,
	}

	signedChars = map[[2]string]bool{
		{"freebsd", "arm"}:   false,
		{"freebsd", "arm64"}: false,
		{"linux", "arm"}:     false,
		{"linux", "arm64"}:   false,
		{"linux", "ppc64le"}: false,
		{"linux", "riscv64"}: false,
		{"linux", "s390x"}:   false,
		{"netbsd", "arm"}:    false,
		{"openbsd", "arm64"}: false,

		{"darwin", "amd64"}:  true,
		{"darwin", "arm64"}:  true,
		{"freebsd", "386"}:   true,
		{"freebsd", "amd64"}: true,
		{"illumos", "amd64"}: true,
		{"linux", "386"}:     true,
		{"linux", "amd64"}:   true,
		{"netbsd", "386"}:    true,
		{"netbsd", "amd64"}:  true,
		{"openbsd", "386"}:   true,
		{"openbsd", "amd64"}: true,
		{"windows", "386"}:   true,
		{"windows", "amd64"}: true,
		{"windows", "arm64"}: true,
	}

	abiTypes = map[[2]string]map[Kind]AbiType{
		// Linux, generated by GCC 8.3.0
		{"linux", "amd64"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Decimal128: {16, 16, 16},
			Decimal32:  {4, 4, 4},
			Decimal64:  {8, 8, 8},
			Double:     {8, 8, 8},
			Enum:       {4, 4, 4},
			Float128:   {16, 16, 16},
			Float32:    {4, 4, 4},
			Float32x:   {8, 8, 8},
			Float64:    {8, 8, 8},
			Float64x:   {16, 16, 16},
			Float:      {4, 4, 4},
			Int128:     {16, 16, 16},
			Int:        {4, 4, 4},
			Long:       {8, 8, 8},
			LongDouble: {16, 16, 16},
			LongLong:   {8, 8, 8},
			Ptr:        {8, 8, 8},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt128:    {16, 16, 16},
			UInt:       {4, 4, 4},
			ULong:      {8, 8, 8},
			ULongLong:  {8, 8, 8},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		{"linux", "386"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Decimal128: {16, 16, 16},
			Decimal32:  {4, 4, 4},
			Decimal64:  {8, 8, 8},
			Double:     {8, 4, 4},
			Enum:       {4, 4, 4},
			Float128:   {16, 16, 16},
			Float32:    {4, 4, 4},
			Float32x:   {8, 4, 4},
			Float64:    {8, 4, 4},
			Float64x:   {12, 4, 4},
			Float:      {4, 4, 4},
			Int:        {4, 4, 4},
			Long:       {4, 4, 4},
			LongDouble: {12, 4, 4},
			LongLong:   {8, 4, 4},
			Ptr:        {4, 4, 4},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt:       {4, 4, 4},
			ULong:      {4, 4, 4},
			ULongLong:  {8, 4, 4},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		{"linux", "arm"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Double:     {8, 8, 8},
			Enum:       {4, 4, 4},
			Float32:    {4, 4, 4},
			Float32x:   {8, 8, 8},
			Float64:    {8, 8, 8},
			Float:      {4, 4, 4},
			Int:        {4, 4, 4},
			Long:       {4, 4, 4},
			LongDouble: {8, 8, 8},
			LongLong:   {8, 8, 8},
			Ptr:        {4, 4, 4},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt:       {4, 4, 4},
			ULong:      {4, 4, 4},
			ULongLong:  {8, 8, 8},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		{"linux", "arm64"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Double:     {8, 8, 8},
			Enum:       {4, 4, 4},
			Float128:   {16, 16, 16},
			Float32:    {4, 4, 4},
			Float32x:   {8, 8, 8},
			Float64:    {8, 8, 8},
			Float64x:   {16, 16, 16},
			Float:      {4, 4, 4},
			Int128:     {16, 16, 16},
			Int:        {4, 4, 4},
			Long:       {8, 8, 8},
			LongDouble: {16, 16, 16},
			LongLong:   {8, 8, 8},
			Ptr:        {8, 8, 8},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt128:    {16, 16, 16},
			UInt:       {4, 4, 4},
			ULong:      {8, 8, 8},
			ULongLong:  {8, 8, 8},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		// gcc (Ubuntu 11.2.0-7ubuntu2) 11.2.0
		{"linux", "riscv64"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Double:     {8, 8, 8},
			Enum:       {4, 4, 4},
			Float128:   {16, 16, 16},
			Float32:    {4, 4, 4},
			Float32x:   {8, 8, 8},
			Float64:    {8, 8, 8},
			Float64x:   {16, 16, 16},
			Float:      {4, 4, 4},
			Function:   {8, 8, 8},
			Int128:     {16, 16, 16},
			Int:        {4, 4, 4},
			Long:       {8, 8, 8},
			LongDouble: {16, 16, 16},
			LongLong:   {8, 8, 8},
			Ptr:        {8, 8, 8},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt128:    {16, 16, 16},
			UInt:       {4, 4, 4},
			ULong:      {8, 8, 8},
			ULongLong:  {8, 8, 8},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		// $ x86_64-w64-mingw32-gcc main.c && wine a.exe
		{"windows", "amd64"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Decimal128: {16, 16, 16},
			Decimal32:  {4, 4, 4},
			Decimal64:  {8, 8, 8},
			Double:     {8, 8, 8},
			Enum:       {4, 4, 4},
			Float128:   {16, 16, 16},
			Float32:    {4, 4, 4},
			Float32x:   {8, 8, 8},
			Float64:    {8, 8, 8},
			Float64x:   {16, 16, 16},
			Float:      {4, 4, 4},
			Int128:     {16, 16, 16},
			Int:        {4, 4, 4},
			Long:       {4, 4, 4},
			LongDouble: {16, 16, 16},
			LongLong:   {8, 8, 8},
			Ptr:        {8, 8, 8},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt128:    {16, 16, 16},
			UInt:       {4, 4, 4},
			ULong:      {4, 4, 4},
			ULongLong:  {8, 8, 8},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		// clang version 14.0.0 (https://github.com/llvm/llvm-project.git 329fda39c507e8740978d10458451dcdb21563be)
		// Target: aarch64-w64-windows-gnu
		{"windows", "arm64"}: {
			Void:       {1, 1, 1},
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			SChar:      {1, 1, 1},
			UChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UShort:     {2, 2, 2},
			Enum:       {4, 4, 4},
			Int:        {4, 4, 4},
			UInt:       {4, 4, 4},
			Long:       {4, 4, 4},
			ULong:      {4, 4, 4},
			LongLong:   {8, 8, 8},
			ULongLong:  {8, 8, 8},
			Ptr:        {8, 8, 8},
			Function:   {8, 8, 8},
			Float:      {4, 4, 4},
			Double:     {8, 8, 8},
			LongDouble: {8, 8, 8},
		},
		// $ i686-w64-mingw32-gcc main.c && wine a.exe
		{"windows", "386"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Decimal128: {16, 16, 16},
			Decimal32:  {4, 4, 4},
			Decimal64:  {8, 8, 8},
			Double:     {8, 8, 8},
			Enum:       {4, 4, 4},
			Float128:   {16, 16, 16},
			Float32:    {4, 4, 4},
			Float32x:   {8, 8, 8},
			Float64:    {8, 8, 8},
			Float64x:   {12, 4, 4},
			Float:      {4, 4, 4},
			Int:        {4, 4, 4},
			Long:       {4, 4, 4},
			LongDouble: {12, 4, 4},
			LongLong:   {8, 8, 8},
			Ptr:        {4, 4, 4},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt:       {4, 4, 4},
			ULong:      {4, 4, 4},
			ULongLong:  {8, 8, 8},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		{"darwin", "amd64"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Double:     {8, 8, 8},
			Enum:       {4, 4, 4},
			Float:      {4, 4, 4},
			Int128:     {16, 16, 16},
			Int:        {4, 4, 4},
			Long:       {8, 8, 8},
			LongDouble: {16, 16, 16},
			LongLong:   {8, 8, 8},
			Ptr:        {8, 8, 8},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt128:    {16, 16, 16},
			UInt:       {4, 4, 4},
			ULong:      {8, 8, 8},
			ULongLong:  {8, 8, 8},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		{"darwin", "arm64"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Double:     {8, 8, 8},
			Enum:       {4, 4, 4},
			Float:      {4, 4, 4},
			Int128:     {16, 16, 16},
			Int:        {4, 4, 4},
			Long:       {8, 8, 8},
			LongDouble: {8, 8, 8},
			LongLong:   {8, 8, 8},
			Ptr:        {8, 8, 8},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt128:    {16, 16, 16},
			UInt:       {4, 4, 4},
			ULong:      {8, 8, 8},
			ULongLong:  {8, 8, 8},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		// gcc (SUSE Linux) 7.5.0
		{"linux", "s390x"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Decimal128: {16, 8, 8},
			Decimal32:  {4, 4, 4},
			Decimal64:  {8, 8, 8},
			Double:     {8, 8, 8},
			Enum:       {4, 4, 4},
			Float128:   {16, 8, 8},
			Float32:    {4, 4, 4},
			Float32x:   {8, 8, 8},
			Float64:    {8, 8, 8},
			Float64x:   {16, 8, 8},
			Float:      {4, 4, 4},
			Int128:     {16, 8, 8},
			Int:        {4, 4, 4},
			Long:       {8, 8, 8},
			LongDouble: {16, 8, 8},
			LongLong:   {8, 8, 8},
			Ptr:        {8, 8, 8},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt128:    {16, 8, 8},
			UInt:       {4, 4, 4},
			ULong:      {8, 8, 8},
			ULongLong:  {8, 8, 8},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		// gcc (FreeBSD Ports Collection) 10.3.0
		{"freebsd", "amd64"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Double:     {8, 8, 8},
			Enum:       {4, 4, 4},
			Float:      {4, 4, 4},
			Int128:     {16, 16, 16},
			Int:        {4, 4, 4},
			Long:       {8, 8, 8},
			LongDouble: {16, 16, 16},
			LongLong:   {8, 8, 8},
			Ptr:        {8, 8, 8},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt128:    {16, 16, 16},
			UInt:       {4, 4, 4},
			ULong:      {8, 8, 8},
			ULongLong:  {8, 8, 8},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		// gcc (FreeBSD Ports Collection) 10.3.0
		{"freebsd", "386"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Double:     {8, 4, 4},
			Enum:       {4, 4, 4},
			Float128:   {16, 16, 16},
			Float32:    {4, 4, 4},
			Float32x:   {8, 4, 4},
			Float64:    {8, 4, 4},
			Float64x:   {16, 16, 16},
			Float:      {4, 4, 4},
			Int:        {4, 4, 4},
			Long:       {4, 4, 4},
			LongDouble: {12, 4, 4},
			LongLong:   {8, 4, 4},
			Ptr:        {4, 4, 4},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt:       {4, 4, 4},
			ULong:      {4, 4, 4},
			ULongLong:  {8, 4, 4},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		// gcc (FreeBSD Ports Collection) 11.3.0
		{"freebsd", "arm"}: {
			Void:       {1, 1, 1},
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			SChar:      {1, 1, 1},
			UChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UShort:     {2, 2, 2},
			Enum:       {4, 4, 4},
			Int:        {4, 4, 4},
			UInt:       {4, 4, 4},
			Long:       {4, 4, 4},
			ULong:      {4, 4, 4},
			LongLong:   {8, 8, 8},
			ULongLong:  {8, 8, 8},
			Ptr:        {4, 4, 4},
			Function:   {4, 4, 4},
			Float:      {4, 4, 4},
			Double:     {8, 8, 8},
			LongDouble: {8, 8, 8},
		},
		// gcc (FreeBSD Ports Collection) 11.3.0
		{"freebsd", "arm64"}: {
			Void:       {1, 1, 1},
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			SChar:      {1, 1, 1},
			UChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UShort:     {2, 2, 2},
			Enum:       {4, 4, 4},
			Int:        {4, 4, 4},
			UInt:       {4, 4, 4},
			Long:       {8, 8, 8},
			ULong:      {8, 8, 8},
			LongLong:   {8, 8, 8},
			ULongLong:  {8, 8, 8},
			Ptr:        {8, 8, 8},
			Function:   {8, 8, 8},
			Float:      {4, 4, 4},
			Double:     {8, 8, 8},
			LongDouble: {16, 16, 16},
			Int128:     {16, 16, 16},
			UInt128:    {16, 16, 16},
		},
		// gcc (GCC) 8.4.0
		{"openbsd", "amd64"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Double:     {8, 8, 8},
			Enum:       {4, 4, 4},
			Float128:   {16, 16, 16},
			Float32:    {4, 4, 4},
			Float32x:   {8, 8, 8},
			Float64:    {8, 8, 8},
			Float64x:   {16, 16, 16},
			Float:      {4, 4, 4},
			Int128:     {16, 16, 16},
			Int:        {4, 4, 4},
			Long:       {8, 8, 8},
			LongDouble: {16, 16, 16},
			LongLong:   {8, 8, 8},
			Ptr:        {8, 8, 8},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt128:    {16, 16, 16},
			UInt:       {4, 4, 4},
			ULong:      {8, 8, 8},
			ULongLong:  {8, 8, 8},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		// OpenBSD clang version 13.0.0
		{"openbsd", "arm64"}: {
			Void:       {1, 1, 1},
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			SChar:      {1, 1, 1},
			UChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UShort:     {2, 2, 2},
			Enum:       {4, 4, 4},
			Int:        {4, 4, 4},
			UInt:       {4, 4, 4},
			Long:       {8, 8, 8},
			ULong:      {8, 8, 8},
			LongLong:   {8, 8, 8},
			ULongLong:  {8, 8, 8},
			Ptr:        {8, 8, 8},
			Function:   {8, 8, 8},
			Float:      {4, 4, 4},
			Double:     {8, 8, 8},
			LongDouble: {16, 16, 16},
			Int128:     {16, 16, 16},
			UInt128:    {16, 16, 16},
		},
		// OpenBSD clang version 13.0.0
		{"openbsd", "386"}: {
			Void:       {1, 1, 1},
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			SChar:      {1, 1, 1},
			UChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UShort:     {2, 2, 2},
			Enum:       {4, 4, 4},
			Int:        {4, 4, 4},
			UInt:       {4, 4, 4},
			Long:       {4, 4, 4},
			ULong:      {4, 4, 4},
			LongLong:   {8, 4, 4},
			ULongLong:  {8, 4, 4},
			Ptr:        {4, 4, 4},
			Function:   {4, 4, 4},
			Float:      {4, 4, 4},
			Double:     {8, 4, 4},
			LongDouble: {12, 4, 4},
		},
		// gcc (GCC) 10.3.0
		{"netbsd", "amd64"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Double:     {8, 8, 8},
			Enum:       {4, 4, 4},
			Float128:   {16, 16, 16},
			Float32:    {4, 4, 4},
			Float32x:   {8, 8, 8},
			Float64:    {8, 8, 8},
			Float64x:   {16, 16, 16},
			Float:      {4, 4, 4},
			Int128:     {16, 16, 16},
			Int:        {4, 4, 4},
			Long:       {8, 8, 8},
			LongDouble: {16, 16, 16},
			LongLong:   {8, 8, 8},
			Ptr:        {8, 8, 8},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt128:    {16, 16, 16},
			UInt:       {4, 4, 4},
			ULong:      {8, 8, 8},
			ULongLong:  {8, 8, 8},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		// gcc (nb4 20200810) 7.5.0
		{"netbsd", "arm"}: {
			Void:       {1, 1, 1},
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			SChar:      {1, 1, 1},
			UChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UShort:     {2, 2, 2},
			Enum:       {4, 4, 4},
			Int:        {4, 4, 4},
			UInt:       {4, 4, 4},
			Long:       {4, 4, 4},
			ULong:      {4, 4, 4},
			LongLong:   {8, 8, 8},
			ULongLong:  {8, 8, 8},
			Ptr:        {4, 4, 4},
			Function:   {4, 4, 4},
			Float:      {4, 4, 4},
			Double:     {8, 8, 8},
			LongDouble: {8, 8, 8},
		},
		// gcc (nb4 20200810) 7.5.0
		{"netbsd", "386"}: {
			Void:       {1, 1, 1},
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			SChar:      {1, 1, 1},
			UChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UShort:     {2, 2, 2},
			Enum:       {4, 4, 4},
			Int:        {4, 4, 4},
			UInt:       {4, 4, 4},
			Long:       {4, 4, 4},
			ULong:      {4, 4, 4},
			LongLong:   {8, 4, 4},
			ULongLong:  {8, 4, 4},
			Ptr:        {4, 4, 4},
			Function:   {4, 4, 4},
			Float:      {4, 4, 4},
			Double:     {8, 4, 4},
			LongDouble: {12, 4, 4},
			Float32:    {4, 4, 4},
			Float32x:   {8, 4, 4},
			Float64:    {8, 4, 4},
			Float64x:   {12, 4, 4},
			Float128:   {16, 16, 16},
		},
		// gcc (Debian 10.2.1-6) 10.2.1 20210110
		{"linux", "ppc64le"}: {
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			Decimal128: {16, 16, 16},
			Decimal32:  {4, 4, 4},
			Decimal64:  {8, 8, 8},
			Double:     {8, 8, 8},
			Enum:       {4, 4, 4},
			Float128:   {16, 16, 16},
			Float32:    {4, 4, 4},
			Float32x:   {8, 8, 8},
			Float64:    {8, 8, 8},
			Float64x:   {16, 16, 16},
			Float:      {4, 4, 4},
			Function:   {8, 8, 8},
			Int128:     {16, 16, 16},
			Int:        {4, 4, 4},
			Long:       {8, 8, 8},
			LongDouble: {16, 16, 16},
			LongLong:   {8, 8, 8},
			Ptr:        {8, 8, 8},
			SChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UChar:      {1, 1, 1},
			UInt128:    {16, 16, 16},
			UInt:       {4, 4, 4},
			ULong:      {8, 8, 8},
			ULongLong:  {8, 8, 8},
			UShort:     {2, 2, 2},
			Void:       {1, 1, 1},
		},
		// gcc (OmniOS 151044/12.2.0-il-0) 12.2.0
		{"illumos", "amd64"}: {
			Void:       {1, 1, 1},
			Bool:       {1, 1, 1},
			Char:       {1, 1, 1},
			SChar:      {1, 1, 1},
			UChar:      {1, 1, 1},
			Short:      {2, 2, 2},
			UShort:     {2, 2, 2},
			Enum:       {4, 4, 4},
			Int:        {4, 4, 4},
			UInt:       {4, 4, 4},
			Long:       {8, 8, 8},
			ULong:      {8, 8, 8},
			LongLong:   {8, 8, 8},
			ULongLong:  {8, 8, 8},
			Ptr:        {8, 8, 8},
			Function:   {8, 8, 8},
			Float:      {4, 4, 4},
			Double:     {8, 8, 8},
			LongDouble: {16, 16, 16},
			Int128:     {16, 16, 16},
			UInt128:    {16, 16, 16},
			Float32:    {4, 4, 4},
			Float32x:   {8, 8, 8},
			Float64:    {8, 8, 8},
			Float64x:   {16, 16, 16},
			Float128:   {16, 16, 16},
		},
	}
)

// ABI describes selected parts of the Application Binary Interface.
type ABI struct {
	ByteOrder binary.ByteOrder
	goarch    string
	goos      string
	Types     map[Kind]AbiType

	SignedChar bool
}

type AbiType struct {
	Size       int64
	Align      int
	FieldAlign int
}

// NewABI creates an ABI based on the os+arch pair.
func NewABI(os, arch string) (*ABI, error) {
	byteOrder, ok := byteOrders[arch]
	if !ok {
		return nil, fmt.Errorf("unsupported arch: %s", arch)
	}

	types0, ok := abiTypes[[2]string{os, arch}]
	if !ok {
		return nil, fmt.Errorf("unsupported os/arch: %s/%s", os, arch)
	}

	types := make(map[Kind]AbiType)
	for k, v := range types0 {
		types[k] = v
	}
	for ck := InvalidKind; ck < maxKind; ck++ {
		rk := correspondingRealKinds[ck]
		if rk == InvalidKind {
			continue
		}

		rt := types[rk]
		ct := AbiType{
			Size:       2 * rt.Size,
			Align:      rt.Align,
			FieldAlign: rt.FieldAlign,
		}
		types[ck] = ct
	}
	return &ABI{
		ByteOrder:  byteOrder,
		SignedChar: signedChars[[2]string{os, arch}],
		goarch:     arch,
		goos:       os,
		Types:      types,
	}, nil
}

func (a *ABI) isSignedInteger(k Kind) bool {
	switch k {
	case Bool, UChar, UInt, ULong, ULongLong, UShort, UInt128:
		return false
	case SChar, Int, Long, LongLong, Short, Int128:
		return true
	case Char:
		return a.SignedChar
	}

	return false
}
