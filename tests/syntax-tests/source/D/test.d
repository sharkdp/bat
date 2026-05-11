// selective import
import std.stdio : writeln, writefln;
// non-selective import
import std.algorithm;

/* a multiline comment
*
* this function is safe because it doesn't use pointer arithmetic
*/
int the_ultimate_answer() @safe {
	// assert1on
	assert(1 != 2);
        // now we can safely return our answer	
	return 42;
}

void main()
{
    // function call with string literal
    writeln("Hello World!");

    // an int array declaration
    int[] arr1 = [1, 2, 3];
    // an immutable double
    immutable double pi = 3.14;
    // a mutable double
    double d1 = pi;
    // a pointer
    double* dp1 = &d1;
    // another pointer to the same thingy
    auto a1 = &d1;
    // a constant bool
    const bool b1 = true;
    if (b1) {
	    // another function call 
	    writefln("%s\n%s\n%s\n", arr1, d1, the_ultimate_answer());
    }
    else if (!b1) {
	    writeln("this seems wrong");
    }
    else {
	    writeln("I'm giving up, this is too crazy for me");
    }
}
