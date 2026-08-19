import { readSettings, writeSettings } from "./config.js";

const saved = document.getElementById("saved");

/** Reflect stored settings into the form. Pre: the DOM is parsed. */
async function load() {
  const { onDuplicate } = await readSettings();
  const input = document.querySelector(`input[value="${onDuplicate}"]`);
  if (input) input.checked = true;
}

for (const input of document.querySelectorAll('input[name="onDuplicate"]')) {
  input.addEventListener("change", async (event) => {
    await writeSettings({ onDuplicate: event.target.value });
    saved.classList.add("show");
    setTimeout(() => saved.classList.remove("show"), 1200);
  });
}

load();
