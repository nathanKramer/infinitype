const app = Elm.Main.init({
  node: document.getElementById("elm"),
  flags: {},
});
const notifyElm = app.ports.command.send;
const commands = ["p"];

function notifyWithDefault(event) {
  event.preventDefault();
  notifyElm(event.key);
}

function keyHandler(event) {
  const isCommand = event.metaKey || event.ctrlKey;

  if (!isCommand) {
    if (event.key === "Tab") {
      notifyWithDefault(event);
      return;
    }

    return;
  }

  commands.forEach((key) => {
    if (event.key === key) {
      notifyWithDefault(event);
    }
  });
}

document.addEventListener("keydown", keyHandler);
