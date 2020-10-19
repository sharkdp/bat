import std.stdio;
import std.algorithm;
import std.range;

/* a multiline comment
*
*/
int the_ultimate_answer() {
	return 42;
}

void main()
{
    // function call with string literal
    writeln("Hello World!");

    // an int array declaration
    int[] arr1 = [1, 2, 3];
    // a double
    double d1 = 3.14;
    // another function call 
    writefln("%s\n%s\n%s\n", arr1, d1, the_ultimate_answer());
}
