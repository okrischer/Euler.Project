package euler

import (
	"testing"
)

func TestProblem003FactorNaive(t *testing.T) {
	want := 6857
	got := factorNaive(600851475143)
	if got != want {
		t.Errorf("got %d, want %d", got, want)
	}
}

func TestProblem003FactorOpt(t *testing.T) {
	want := 6857
	got := factorOpt(600851475143)
	if got != want {
		t.Errorf("got %d, want %d", got, want)
	}
}

func BenchmarkProblem003FactorNaive(b *testing.B) {
	for i := 0; i < b.N; i++ {
		factorNaive(600851475143)
	}
}

func BenchmarkProblem003FactorOpt(b *testing.B) {
	for i := 0; i < b.N; i++ {
		factorOpt(600851475143)
	}
}
