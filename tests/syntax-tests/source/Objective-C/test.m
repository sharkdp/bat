#import <myClass.h>
@import Foundation
// Single line comments

/*
 * Multi line comment
 */

int main (int argc, const char * argv[]) {
   NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

   NSLog(@"Storage size for int : %d \n", sizeof(int));
   NSLog (@"hello world");
   if (NO)
   {
       NSLog(@"I am never run");
   } else if (0)
   {
       NSLog(@"I am also never run");
   } else
   {
       NSLog(@"I print");
   }

   [pool drain];
   return 0;
}

@implementation MyClass {
    long distance;
    NSNumber height;
}
