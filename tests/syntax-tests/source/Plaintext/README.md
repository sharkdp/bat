This text file was generated with the following script.

```python
with open("plaintext.txt", "w"):
    for i in range(176):
        try:
            f.write(chr(i) + "\n")
        except:
            pass
    f.write("\n")
    f.write("Here is a line with multiple characters\n")
```
