import type { IDisposable, ITerminalAddon, Terminal } from "@xterm/xterm";
import _ from "lodash";
import wcwidth from "wcwidth";

const ansiRegex =
  // deno-lint-ignore no-control-regex
  /[\u001b\u009b][[()#;?](?:[0-9]{1,4}(?:;[0-9]{0,4}))?[0-zARZcfqnyjk]/g;

export default class Readline implements ITerminalAddon {
  private term: Terminal | undefined;
  private disposables = [] as IDisposable[];
  private logic = { x: 0, y: 0 };
  private buffer = [[]] as string[][];
  private cursor = { x: 0, y: 0 };
  private prompt: string[];
  // passing null to print first prompt
  // return value indicating if the line input is completed
  private onLine: (line: string | null) => Promise<boolean>;
  private waiting = false;
  private incompPrompt: string[];
  private history = [] as string[][][];
  private historyIdx = 0;

  constructor(
    prompt: string,
    onLine: (
      line: string | null,
    ) => Promise<boolean>,
  ) {
    this.prompt = [...prompt];
    this.incompPrompt = Array(wcwidth(prompt)).fill(" ");
    this.onLine = onLine;
  }

  activate(term: Terminal) {
    this.term = term;
    this.incompPrompt = Array(this.countVisual(this.prompt)).fill(" ");
    this.onLine(null).then(() => {
      this.redraw();
      this.disposables.push(term.onData((data) => this.handleData(data)));
      this.disposables.push(term.onResize(() => this.redraw()));
    });
  }

  dispose() {
    this.disposables.forEach((disposable) => disposable.dispose());
  }

  private countVisual(text: string[]) {
    if (!this.term) {
      return 0;
    }
    const cleaned = text.join("").replace(ansiRegex, "");
    const cols = this.term.cols;
    let col = 0;
    let lines = 0;
    for (const char of cleaned) {
      const w = wcwidth(char);
      if (w <= 0) {
        continue;
      }
      if (col + w > cols) {
        lines++;
        col = w;
      } else {
        col += w;
      }
    }
    return lines * cols + col;
  }

  private onReturn() {
    if (this.empty()) {
      this.term?.write("\r\n");
      this.clear();
      this.redraw();
      return;
    }
    this.waiting = true;
    const line = this.buffer.map((line) => line.join("")).join("\n");
    this.term?.write("\x1b[s");
    this.term?.write("\r\n");
    this.onLine(line).then(
      (completed) => {
        if (!this.term) {
          return;
        }
        if (completed) {
          this.historyPush();
          this.clear();
        } else {
          this.term?.write("\x1b[1u");
          this.buffer.push([]);
          this.logic.y++;
          this.logic.x = 0;
        }
        this.redraw();
        this.waiting = false;
      },
    );
  }

  private backspace() {
    if (this.logic.x > 0) {
      this.logic.x--;
      this.buffer[this.logic.y].splice(this.logic.x, 1);
      this.redraw();
    }
  }

  private cursorLeft() {
    if (this.logic.x > 0) {
      this.logic.x--;
      this.redraw();
    }
  }

  private cursorRight() {
    if (this.logic.x < this.buffer[this.logic.y].length) {
      this.logic.x++;
      this.redraw();
    }
  }

  clear() {
    this.buffer = [[]];
    this.logic = { x: 0, y: 0 };
    this.cursor = { x: 0, y: 0 };
  }

  private empty() {
    return this.buffer.map((line) => line.join("")).join("\n").trim() === "";
  }

  private historyUp() {
    if (this.historyIdx === 0) {
      return;
    }
    if (this.historyIdx === this.history.length) {
      this.history.push(structuredClone(this.buffer));
    }
    this.historyIdx--;
    this.buffer = structuredClone(this.history[this.historyIdx]);
    this.logic.y = this.buffer.length - 1;
    this.logic.x = this.buffer[this.logic.y].length;
    this.redraw();
  }

  private historyPush() {
    if (_.isEqual(this.history[this.historyIdx], this.buffer)) {
      this.historyIdx = this.history.length;
      return;
    }
    this.history.length = this.historyIdx;
    this.history.push(structuredClone(this.buffer));
    this.historyIdx++;
  }

  private historyDown() {
    if (this.historyIdx >= this.history.length - 1) {
      return;
    }
    this.historyIdx++;
    this.buffer = structuredClone(this.history[this.historyIdx]);
    this.logic.y = this.buffer.length - 1;
    this.logic.x = this.buffer[this.logic.y].length;
    this.redraw();
    if (this.historyIdx === this.history.length - 1) {
      this.history.length--;
    }
  }

  private handleData(data: string) {
    if (this.waiting) {
      return;
    }
    switch (data) {
      // return
      case "\r":
        this.onReturn();
        break;

      // backspace
      case "\x7f":
        this.backspace();
        break;

      // left
      case "\x1b[D":
        this.cursorLeft();
        break;

      // right
      case "\x1b[C":
        this.cursorRight();
        break;

      // up
      case "\x1b[A":
        this.historyUp();
        break;

      // down
      case "\x1b[B":
        this.historyDown();
        break;

      // ^C
      case "\x03":
        this.term?.write("^C\r\n");
        this.clear();
        this.redraw();
        break;

      default:
        this.insert(data);
    }
  }

  redraw() {
    if (!this.term) {
      return;
    }

    const term = this.term;
    term.write("\x1b[s");
    if (this.cursor.y > 0) {
      term.write(`\x1b[${this.cursor.y}A`);
    }
    term.write("\r");
    term.write("\x1b[0J");
    const written = this.buffer.map((line, i) =>
      (i === 0 ? this.prompt : this.incompPrompt).concat(line)
    );
    term.write(written.map((line) => line.join("")).join("\r\n"));
    term.write("\x1b[u");

    this.alignCursor();
  }

  private alignCursor() {
    if (!this.term) {
      return;
    }

    const term = this.term;

    const written = this.buffer.map((line, i) =>
      (i === 0 ? this.prompt : this.incompPrompt).concat(line)
    );
    const counts = written.map((line) => this.countVisual(line));
    const lines = counts.map((count) => Math.ceil(count / term.cols));

    let realY = lines.slice(0, this.logic.y).reduce(
      (acc, line) => acc + line,
      0,
    );

    const prefix = this.logic.y === 0 ? this.prompt : this.incompPrompt;
    const inLine = this.countVisual(prefix.concat(
      this.buffer[this.logic.y].slice(0, this.logic.x),
    ));
    if (inLine !== 0) {
      realY += Math.floor(inLine / term.cols);
    }
    if (realY > this.cursor.y) {
      term.write(`\x1b[${realY - this.cursor.y}B`);
    } else if (realY < this.cursor.y) {
      term.write(`\x1b[${this.cursor.y - realY}A`);
    }
    this.cursor.y = realY;

    const realX = inLine % term.cols;
    if (realX > this.cursor.x) {
      term.write(`\x1b[${realX - this.cursor.x}C`);
    } else if (realX < this.cursor.x) {
      term.write(`\x1b[${this.cursor.x - realX}D`);
    }
    this.cursor.x = realX;
  }

  private insert(data: string) {
    const chars = [...data];
    this.buffer[this.logic.y].splice(this.logic.x, 0, ...chars);
    this.logic.x += chars.length;
    this.redraw();
  }
}
