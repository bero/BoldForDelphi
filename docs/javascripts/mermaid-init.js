// Initialize Mermaid with loose security to enable click links in diagrams
document.addEventListener("DOMContentLoaded", function () {
  mermaid.initialize({
    startOnLoad: true,
    securityLevel: "loose",
    theme: "default",
  });
});
