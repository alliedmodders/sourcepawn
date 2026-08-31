// Minimal reproduction - referencing a function in a branch condition leaked
// the SpFunction when the branch's lowering emitted LL_RELEASE after the
// unconditional fallthrough jump, leaving the release unreachable on both
// paths. Run spshell with --leak-check to see the LEAK DETECTED report.
public void main() {
    if (main != INVALID_FUNCTION) {}
    if (main != INVALID_FUNCTION) {}
}
