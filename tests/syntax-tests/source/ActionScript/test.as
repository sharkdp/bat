import flash.events.*;
import flash.events.MouseEvent;

package TestSyntax {
    public class TestSyntax  extends flash.display.Sprite {

        public static const TEST_CONSTANT:Number = 33.333;

        var testAttribute:int = 1;

        public namespace TestNamespace;
        TestNamespace function Method2():void { }

        /**
         * Multi-line comment
         */
        override public function set x(value:Number):void
        {
            super.x = Math.round(value);
        }

        /**
         * Actual multi-line comment
         * Takes up multiple lines
         */
        override public function set y(value:Number):void
        {
            super.y = 0;
        }

        public function testFunction() {
            var test:String = 'hello';

            // arrays
            var testArray:Array = ["a", "b", "c", "d"];
            for (var i:uint = 0; i < testArray.length; i++)
                trace(testArray[i]);

            // objects
            var testObject:Object = {foo: 20, bar: 40};
            for (var key:String in testObject) {
                trace(testObject[key]);
            }
            for each (var objectValue:int in testObject) {
                trace(objectValue);
            }

            // dynamic variables
            var testDynamic:*;
            testDynamic = 75;
            testDynamic = "Seventy-five";

            // regex
            var testRegExp:RegExp = /foo-\d+/i;

            // XML
            var testXML:XML =
<employee>
  <firstName>Harold</firstName>
  <lastName>Webster</lastName>
</employee>;
        }

        private function anotherFunc(a:int, arg2:uint, arg3:Function, ... args) {

        }

        [Embed(source="sound1.mp3")] public var soundCls:Class;
        public function SoundAssetExample()
        {
            var mySound:SoundAsset = new soundCls() as SoundAsset;
            var sndChannel:SoundChannel = mySound.play();
        }
    }
}
