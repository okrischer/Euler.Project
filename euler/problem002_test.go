package euler

import (
	"testing"
)

func TestProblem002FibImp(t *testing.T) {
	want := 4613732
	got := fibImp(4000000)
	if got != want {
		t.Errorf("got %d, want %d", got, want)
	}
}

func TestProblem002FibOpt(t *testing.T) {
	want := 4613732
	got := fibOpt(4000000)
	if got != want {
		t.Errorf("got %d, want %d", got, want)
	}
}

func BenchmarkProblem002FibImp(b *testing.B) {
	for i := 0; i < b.N; i++ {
		fibImp(4000000)
	}
}

func BenchmarkProblem002FibOpt(b *testing.B) {
	for i := 0; i < b.N; i++ {
		fibOpt(4000000)
	}
}
