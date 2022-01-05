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

// Typing updates.
// We do this outside of elm to assist with supporting text composition (chinese/japanese input)
infinitype.addEventListener("compositionstart", function (event) {
  console.log(
    "Composition Start",
    JSON.stringify(event),
    event.data,
    event.target.value
  );
  interface.composingInput(true);
});
infinitype.addEventListener("compositionupdate", function (event) {
  console.log("composition update", JSON.stringify(event), event.target.value);
});
infinitype.addEventListener("compositionend", function (event) {
  interface.composingInput(false);
  console.log("Composition End", JSON.stringify(event), event.target.value);
  interface.onChange(event.target.value);
});
