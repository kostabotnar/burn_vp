# Project instructions

## File output

- Code in this project writes files only inside the `output/` folder. It must not write to the home folder, the Desktop, the temporary folder, or any other location.
- This includes test or probe files used to check whether a folder is writable.
- Locate `output/` relative to the project root, not the working directory or the home folder.
- If `output/` does not exist, create it at the project root. Never create it anywhere else.
- `PROJECT_VERSION` at the root identifies the project folder; code locates the root by it and must not write if it is not found.
