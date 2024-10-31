# Markdown To HTML Parser

## Overview

This project is a Markdown to HTML converter developed as a side project. The project uses Haskell for the backend and TypeScript with RxJS for the frontend. The backend parses Markdown and converts it to HTML, while the frontend provides an interactive interface for users to input Markdown and view the generated HTML.

## Features

- **Markdown Parsing**: Convert Markdown text to HTML.
- **Interactive Interface**: Input Markdown and view the HTML output in real-time.
- **Save Functionality**: Save the generated HTML to the backend.
- **Syntax Highlighting**: Highlight code blocks in the HTML output.

## Installation

To set up the project locally, follow these steps:

1. **Clone the repository**:

```sh
git clone https://github.com/vinmeil/markdown-parser.git
cd <repository-directory>
```

2. **Install dependencies for the frontend**:

```sh
cd JS
npm install
```

## Usage

1. **Start the backend server**:

```sh
cd Haskell
stack run
```

2. **Start the frontend development server**:

```sh
cd JS
npm run dev
```

3. **Open the application in your browser**:
   Navigate to `http://localhost:5173` (or the port specified by Vite).

4. **Use the application**:
   - Enter Markdown text in the input area.
   - View the generated HTML in the output area.
   - Click the "Save" button to save the HTML to the backend.

## Key Files

- **Assignment.hs**: Contains the Markdown parsing logic.
- **Main.hs**: Main entry point for the Haskell backend.
- **main.ts**: Main entry point for the frontend logic.
- **types.ts**: Type definitions and constants for the frontend.

## Technologies Used

- **TypeScript**: For type-safe JavaScript.
- **RxJS**: For functional reactive programming.
- **Haskell**: For the backend.
- **Vite**: For fast development and build tooling.

## Contributing

Contributions are welcome! Please follow these steps to contribute:

1. **Fork the repository**.
2. **Create a new branch**:
   ```sh
   git checkout -b feature/your-feature-name
   ```
3. **Make your changes**.
4. **Commit your changes**:
   ```sh
   git commit -m 'Add some feature'
   ```
5. **Push to the branch**:
   ```sh
   git push origin feature/your-feature-name
   ```
6. **Open a pull request**.

## License

This project is not licensed and is intended for educational purposes. Please do not use it to create a public product. If you wish to use it for private, educational purposes, please contact the authors.

## Authors

- Vincent Wesley Liem - [vinmeil](https://github.com/vinmeil) - Main author
- Adrian Kristanto - [adriankristanto](https://github.com/adriankristanto) - Provided the scaffold in which the repo was forked from

## Contact

For any questions or feedback, please contact Vincent Liem at vincent.wesley.liem@gmail.com.
