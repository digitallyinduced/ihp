import { AnsiUp } from "/vendor/ansi-up.min.js";

function renderAnsiLogs() {
  const pres = document.querySelectorAll('#logs pre');
  if (pres.length === 0) return;

  const ansiUp = new AnsiUp();
  ansiUp.use_classes = false;

  pres.forEach(pre => {
    const ESC = "\u001b";
    const text = pre.textContent.replace(/\\ESC\[/g, ESC + "[");
    const html = ansiUp.ansi_to_html(text);

    pre.innerHTML = html;
  });
}
document.addEventListener('turbolinks:load', renderAnsiLogs);