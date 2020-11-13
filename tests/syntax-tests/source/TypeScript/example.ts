let letNumber = 10;
const constNumber = 20;

const bool: boolean = true;
const list: number[] = [1, 2, 3];
const array: Array<number> = [1, 2, 3];
const pair: [string, number] = ['hello', 10];

for (let i = 0; i < list.length; i += 1) {
  console.log(list[i]);
}

if (bool) {
  console.log('True');
} else {
  console.log('False');
}

const str: string = 'Jake';
const templateStr: string = `Hello, ${str}!`;

// A comment

/*
 * Multiline comments
 * Multiline comments
 */

interface SquareConfig {
  label: string;
  color?: string;
  width?: number;
  [propName: string]: any;
}

interface SearchFunc {
  (source: string, subString: string): boolean;
}

enum Color {
  Red,
  Green,
}

type Easing = "ease-in" | "ease-out" | "ease-in-out";

class Greeter {
  private readonly greeting: string;

  constructor(message: string) {
    this.greeting = message;
  }

  greet() {
    return "Hello, " + this.greeting;
  }
}

let greeter = new Greeter("world");

class Animal {
  move(distanceInMeters: number = 0) {
    console.log(`Animal moved ${distanceInMeters}m.`);
  }
}

class Dog extends Animal {
  bark() {
    console.log("Woof! Woof!");
  }
}

const dog = new Dog();
dog.bark();
dog.move(10);
dog.bark();

class Point {
  x: number;
  y: number;
}

interface Point3d extends Point {
  z: number;
}

let point3d: Point3d = { x: 1, y: 2, z: 3 };

function add(x, y) {
  return x + y;
}

let myAdd = function (x, y) {
  return x + y;
};

(function () {
  console.log('IIFE');
}());

function identity<T>(arg: T): T {
  return arg;
}

let myIdentity: <T>(arg: T) => T = identity;

class GenericNumber<T> {
  zeroValue: T;
  add: (x: T, y: T) => T;
}
