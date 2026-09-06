# Makefile fences

```makefile
CC := cc
all: app
	$(CC) -o $@ $<
```

~~~Makefile
.PHONY: clean
clean:
	rm -f app
~~~

```make
include config.mk
```

```mk
SOURCES = main.c
```

**Markdown resumes after the fence.**
