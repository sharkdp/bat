#!/usr/bin/env python3
"""Read a version 4 Jupyter notebook from stdin and write a Markdown preview.

Usage: bat --process "python3 /path/to/notebook-preview.py" -l markdown file.ipynb
Format: https://nbformat.readthedocs.io/en/latest/format_description.html
Uses only the Python standard library. Cells are never executed.
"""

import json
import re
import sys


def multiline(value):
    if isinstance(value, str):
        return value
    if isinstance(value, list) and all(isinstance(part, str) for part in value):
        return "".join(value)
    raise ValueError("expected a string or a list of strings")


def fenced(text, language="text"):
    length = max([2] + [len(run) for run in re.findall(r"`+", text)]) + 1
    fence = "`" * length
    return f"{fence}{language}\n{text}" + ("" if text.endswith("\n") else "\n") + f"{fence}\n"


def object_value(value, description):
    if not isinstance(value, dict):
        raise ValueError(f"expected an object for {description}")
    return value


def output_preview(output):
    output = object_value(output, "cell output")
    kind = output.get("output_type")
    if kind == "stream":
        return fenced(multiline(output.get("text", "")))
    if kind in ("display_data", "execute_result"):
        data = object_value(output.get("data", {}), "output data")
        if "text/plain" in data:
            return fenced(multiline(data["text/plain"]))
        if "text/markdown" in data:
            return multiline(data["text/markdown"])
        return fenced("[Output: " + (", ".join(sorted(data)) or "empty") + "]")
    if kind == "error":
        frames = output.get("traceback", [])
        if not isinstance(frames, list) or not all(isinstance(frame, str) for frame in frames):
            raise ValueError("expected a list of strings for traceback")
        text = "\n".join(frames) if frames else f"{output.get('ename', 'Error')}: {output.get('evalue', '')}"
        return fenced(text)
    return fenced(f"[Unsupported output type: {kind}]")


def preview(notebook):
    notebook = object_value(notebook, "notebook")
    if notebook.get("nbformat") != 4:
        raise ValueError("only notebook format 4 is supported")
    cells = notebook.get("cells")
    if not isinstance(cells, list):
        raise ValueError("expected a list of cells")
    metadata = object_value(notebook.get("metadata", {}), "metadata")
    info = object_value(metadata.get("language_info", {}), "language_info")
    kernel = object_value(metadata.get("kernelspec", {}), "kernelspec")
    language = info.get("name", kernel.get("language", "text"))
    if not isinstance(language, str) or not re.fullmatch(r"[A-Za-z0-9_+.-]+", language):
        language = "text"
    sections = []
    for index, cell in enumerate(cells, 1):
        cell = object_value(cell, "cell")
        kind = cell.get("cell_type")
        source = multiline(cell.get("source", ""))
        sections.append(f"## Cell {index}\n")
        if kind == "markdown":
            sections.append(source)
        elif kind == "raw":
            sections.append(fenced(source))
        elif kind == "code":
            sections.append(fenced(source, language))
            outputs = cell.get("outputs", [])
            if not isinstance(outputs, list):
                raise ValueError("expected a list of outputs")
            sections.extend(output_preview(output) for output in outputs)
        else:
            sections.append(fenced(f"[Unsupported cell type: {kind}]\n{source}"))
    return "\n\n".join(section.rstrip("\n") for section in sections) + ("\n" if sections else "")


def main():
    try:
        # Render fully before writing, so invalid input cannot leave a partial preview.
        text = preview(json.load(sys.stdin.buffer))
        sys.stdout.buffer.write(text.encode("utf-8"))
    except BrokenPipeError:
        # bat may intentionally stop reading after the requested line range.
        return 0
    except (ValueError, RecursionError) as error:
        print(f"notebook-preview: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
