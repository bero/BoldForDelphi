import { Button } from "@/components/ui/button";
import { Github, ExternalLink, Mail } from "lucide-react";

export function Header() {
  return (
    <header className="fixed top-0 left-0 right-0 z-50 px-6 py-4">
      <nav className="max-w-7xl mx-auto flex items-center justify-between glass-effect rounded-2xl px-6 py-3">
        <a href="/" className="flex items-center gap-3 group">
          <div className="w-10 h-10 rounded-xl bg-gradient-primary flex items-center justify-center shadow-lg group-hover:scale-105 transition-transform">
            <span className="text-lg font-bold text-primary-foreground">B</span>
          </div>
          <span className="font-semibold text-lg hidden sm:block">Bold for Delphi</span>
        </a>

        <div className="hidden md:flex items-center gap-8">
          <a href="#demos" className="text-sm text-muted-foreground hover:text-foreground transition-colors">
            Demos
          </a>
          <a href="#features" className="text-sm text-muted-foreground hover:text-foreground transition-colors">
            Features
          </a>
          <a href="#docs" className="text-sm text-muted-foreground hover:text-foreground transition-colors">
            Documentation
          </a>
        </div>

        <div className="flex items-center gap-3">
          <Button variant="ghost" size="icon" asChild className="hidden sm:flex">
            <a href="https://github.com/Embarcadero/BoldForDelphi" target="_blank" rel="noopener noreferrer">
              <Github className="w-5 h-5" />
            </a>
          </Button>
          <Button variant="glow" size="sm" asChild>
            <a href="https://github.com/Embarcadero/BoldForDelphi" target="_blank" rel="noopener noreferrer">
              Get Started
              <ExternalLink className="w-3.5 h-3.5 ml-1" />
            </a>
          </Button>
        </div>
      </nav>
    </header>
  );
}

export function Footer() {
  return (
    <footer className="py-12 px-6 border-t border-border">
      <div className="max-w-6xl mx-auto">
        <div className="grid grid-cols-1 md:grid-cols-4 gap-8 mb-8">
          <div className="md:col-span-2">
            <div className="flex items-center gap-3 mb-4">
              <div className="w-10 h-10 rounded-xl bg-gradient-primary flex items-center justify-center">
                <span className="text-lg font-bold text-primary-foreground">B</span>
              </div>
              <span className="font-semibold text-lg">Bold for Delphi</span>
            </div>
            <p className="text-muted-foreground text-sm max-w-sm">
              A powerful Model-Driven Architecture framework for Embarcadero Delphi, 
              enabling rapid enterprise application development.
            </p>
          </div>

          <div>
            <h4 className="font-semibold mb-4">Resources</h4>
            <ul className="space-y-2 text-sm text-muted-foreground">
              <li><a href="#demos" className="hover:text-foreground transition-colors">Demo Applications</a></li>
              <li><a href="#docs" className="hover:text-foreground transition-colors">Documentation</a></li>
              <li><a href="#" className="hover:text-foreground transition-colors">Tutorials</a></li>
              <li><a href="#" className="hover:text-foreground transition-colors">FAQ</a></li>
            </ul>
          </div>

          <div>
            <h4 className="font-semibold mb-4">Community</h4>
            <ul className="space-y-2 text-sm text-muted-foreground">
              <li>
                <a href="https://github.com/Embarcadero/BoldForDelphi" target="_blank" rel="noopener noreferrer" className="hover:text-foreground transition-colors flex items-center gap-2">
                  <Github className="w-4 h-4" /> GitHub
                </a>
              </li>
              <li>
                <a href="mailto:info@boldfordelphi.com" className="hover:text-foreground transition-colors flex items-center gap-2">
                  <Mail className="w-4 h-4" /> Contact
                </a>
              </li>
            </ul>
          </div>
        </div>

        <div className="pt-8 border-t border-border flex flex-col md:flex-row justify-between items-center gap-4 text-sm text-muted-foreground">
          <p>© 2024 Bold for Delphi. Open source under MIT License.</p>
          <p>Originally developed by BoldSoft MDE AB</p>
        </div>
      </div>
    </footer>
  );
}
