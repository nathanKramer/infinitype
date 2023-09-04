// We must not crash

const CORPUS_KEY = "infinitype:chosen_corpus";
const DEFAULT_CORPUS = 6;
const initialCorpus =
  parseInt(localStorage.getItem(CORPUS_KEY)) || DEFAULT_CORPUS;

const app = Elm.Main.init({
  node: document.getElementById("elm"),
  flags: { corpus: initialCorpus },
});
const infinitype = document.getElementById("infinitype");

const interface = {
  notify: app.ports.command.send,
  commands: ["p"],
  prevent: ["ArrowLeft", "ArrowRight"],
};

app.ports.corpusChanged.subscribe(function (corpusIndex) {
  localStorage.setItem(CORPUS_KEY, corpusIndex);
});

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
