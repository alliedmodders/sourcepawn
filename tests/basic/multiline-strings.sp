#include <shell>

public void main()
{
	print("""
		Hello
		There
		""");
	// -> "Hello\nThere"

	print("\n---\n");

	print("""
	  Hello
	  There
	""");
	// -> "  Hello\n  There"

	print("\n---\n");

	printf(
		"""""
		"""ASDF""" %d
		""""",
		5
	);
	// -> "\"\"\"ASDF\"\"\" %d" (which prints `"""ASDF""" 5`)

	print("\n---\n");

	print("""test hello "world", how are you?""");
	// -> "test hello \"world\", how are you?"

	print("\n---\n");

	print(""" test \
	wow!\
	""");
	// -> " test \twow!\t"

	print("\n---\n");

	// This one is funky!
	print("""abc\
		"WOAH"\
		""");
	// -> "abc\t\t\"WOAH\"\t\t"

	print("\n---\n");

	print("""
	""");
	// -> ""

	print("\n---\n");

	print("""
	test

	""");
	// -> "test\n\n"

	print("\n---\n");

	print("""

	a
	""");
	// -> "\na"

	print("\n---\n");

	print("""

	""");
	// -> "\n"

	print("\n---\n");

	print("""


	""");
	// -> "\n\n"

	print("\n---\n");
}
