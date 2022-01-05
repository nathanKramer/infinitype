// We must not crash

const app = Elm.Main.init({
  node: document.getElementById("elm"),
  flags: {},
});
const infinitype = document.getElementById("infinitype");

const interface = {
  notify: app.ports.command.send,
  onChange: app.ports.onChange.send,
  composingInput: app.ports.composingInput.send,
  commands: ["p"],
  prevent: ["ArrowLeft", "ArrowRight"],
};

function notifyWithDefault(event) {
  event.preventDefault();
  interface.notify(event.key);
}

function keyHandler(event) {
  const isCommand = event.metaKey || event.ctrlKey;
  const { prevent, commands } = interface;

  if (!isCommand) {
    if (event.key === "Tab") {
      notifyWithDefault(event);
      return;
    }

    prevent.forEach((key) => {
      if (event.key === key) event.preventDefault();
    });

    return;
  }

  commands.forEach((key) => {
    if (event.key === key) {
      notifyWithDefault(event);
    }
  });
}

document.addEventListener("keydown", keyHandler);
