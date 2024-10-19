import { fromEvent, merge, of } from "rxjs";
import {
    map,
    mergeScan,
    first,
    tap,
    mergeWith,
    mergeMap,
    switchMap,
} from "rxjs/operators";
import { ajax } from "rxjs/ajax";
import { type Observable } from "rxjs";
import { State } from "./types";

import hljs from "highlight.js/lib/core";

import javascript from "highlight.js/lib/languages/javascript";
import python from "highlight.js/lib/languages/python";
import haskell from "highlight.js/lib/languages/haskell";

// Load the languages from the unit for syntax highlighting!
hljs.registerLanguage("javascript", javascript);
hljs.registerLanguage("python", python);
hljs.registerLanguage("haskell", haskell);

const markdownInput = document.getElementById(
    "markdown-input",
) as HTMLTextAreaElement;
const checkbox = document.querySelector('input[name="checkbox"]')!;
const titleInput = document.getElementById("title-input") as HTMLInputElement;
const saveButton = document.getElementById("save-button") as HTMLButtonElement;

type Action = (_: State) => State;

const resetState: Action = (s) => {
    return { ...s, save: false };
};

const compose =
    <T, U>(g: (_: T) => U) =>
    <V>(f: (_: U) => V) =>
    (t: T): V =>
        f(g(t));

// Create an Observable for keyboard input events
const input$: Observable<Action> = fromEvent<KeyboardEvent>(
    markdownInput,
    "input",
).pipe(
    map((event) => (event.target as HTMLInputElement).value),
    mergeWith(of(markdownInput.value)),
    map((value) => (s) => ({ ...s, markdown: value, save: false })),
);

const titleInput$: Observable<Action> = fromEvent<KeyboardEvent>(
    titleInput,
    "input",
).pipe(
    map((event) => (event.target as HTMLInputElement).value),
    mergeWith(of(titleInput.value)),
    map((value) => (s) => ({ ...s, title: value })),
);

const saveButton$ = fromEvent<Action>(saveButton, "mousedown").pipe(
    map((_) => (s: State) => ({ ...s, save: true })),
);

const checkboxStream$: Observable<Action> = fromEvent(checkbox, "change").pipe(
    map((event) => (event.target as HTMLInputElement).checked),
    map((value) => (s) => ({ ...s, renderHTML: value })),
);

function getHTML(s: State): Observable<State> {
    // Get the HTML as a stream
    return ajax<{ html: string }>({
        url: "/api/convertMD",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: [s.markdown, s.title].join("💀"),
    }).pipe(
        map((response) => response.response), // Extracting the response data
        map((data) => {
            return {
                ...s,
                HTML: data.html,
            };
        }),
        first(),
    );
}

function saveHTML(s: State): Observable<State> {
    console.log("in here");
    return ajax<{ success: string }>({
        url: "/api/saveHTML",
        method: "POST",
        headers: {
            "Content-Type": "text/plain",
        },
        body: s.HTML,
    }).pipe(
        map((response) => response.response),
        map((_) => s),
        first(),
    );
}

const initialState: State = {
    markdown: "",
    HTML: "",
    renderHTML: true,
    save: false,
    title: "",
};

function main() {
    // Subscribe to the input Observable to listen for changes
    const subscription = merge(
        input$,
        titleInput$,
        checkboxStream$,
        saveButton$,
    )
        .pipe(
            // this messes up the state because it keeps setting save to false
            // map((reducer: Action) => {
            //     // Reset Some variables in the state in every tick
            //     const newReducer = compose(reducer)(resetState);
            //     return newReducer;
            // }),
            mergeScan((acc: State, reducer: Action) => {
                const newState = reducer(acc);
                // getHTML returns an observable of length one
                // so we `scan` and merge the result of getHTML in to our stream
                return getHTML(newState);
            }, initialState),
            switchMap((state: State) => {
                if (state.save) {
                    return saveHTML(state).pipe(
                        map((savedState) => ({
                            ...savedState,
                            save: false, // Reset the save flag after saving
                        })),
                    );
                }
                return of(state);
            }),
        )
        .subscribe((value) => {
            const htmlOutput = document.getElementById(
                "html-output",
            ) as HTMLTextAreaElement | null;
            console.log(value);
            const htmlRender = document.getElementById("html-render");
            if (htmlOutput) {
                htmlOutput.innerHTML = "";
                htmlOutput.textContent = "";
                htmlOutput.textContent = value.HTML;
                htmlOutput.value = value.HTML;
            }
            if (htmlRender) {
                htmlRender.innerHTML = "";
                htmlRender.textContent = "";
                const highlight =
                    '<link rel="stylesheet" href="https://unpkg.com/@highlightjs/cdn-assets@11.3.1/styles/default.min.css" />';
                htmlRender.innerHTML = highlight + value.HTML;
                const blocks = htmlRender.querySelectorAll("pre code");
                blocks.forEach((block) =>
                    hljs.highlightElement(block as HTMLElement),
                );
            }
        });
}
if (typeof window !== "undefined") {
    window.onload = function () {
        main();
    };
}
