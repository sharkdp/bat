// Numbers, strings, keywords, properties and function names.
export class Greeting {
  constructor(name) {
    this.name = name;
    this.count = 42;
  }

  message() {
    const pattern = /hello\s+world/i;
    return `Hello, ${this.name}!`;
  }
}
