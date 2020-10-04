This text file was generated with the following script. Certain invalid characters were removed manually:

```python
with open("plaintext.txt", "w"):
    for i in range(0x10FFFF):
        try:
            f.write(chr(i) + "\n")
        except:
            pass
```
