import { Button } from "@/components/ui/button";
import { ArrowDown, Github, BookOpen } from "lucide-react";

export function HeroSection() {
  return (
    <section className="relative min-h-screen flex items-center justify-center px-6 overflow-hidden">
      {/* Background effects */}
      <div className="absolute inset-0 bg-gradient-hero" />
      <div className="absolute top-1/4 left-1/4 w-96 h-96 bg-primary/10 rounded-full blur-3xl animate-pulse-glow" />
      <div className="absolute bottom-1/4 right-1/4 w-80 h-80 bg-accent/10 rounded-full blur-3xl animate-pulse-glow" style={{ animationDelay: '1.5s' }} />
      
      {/* Grid pattern overlay */}
      <div 
        className="absolute inset-0 opacity-[0.03]"
        style={{
          backgroundImage: `linear-gradient(hsl(var(--primary)) 1px, transparent 1px), linear-gradient(90deg, hsl(var(--primary)) 1px, transparent 1px)`,
          backgroundSize: '60px 60px'
        }}
      />

      <div className="relative z-10 max-w-5xl mx-auto text-center">
        {/* Badge */}
        <div className="inline-flex items-center gap-2 px-4 py-2 rounded-full glass-effect mb-8 opacity-0 animate-fade-in">
          <span className="w-2 h-2 rounded-full bg-primary animate-pulse" />
          <span className="text-sm text-muted-foreground">Open Source • MIT License</span>
        </div>

        {/* Main title */}
        <h1 className="text-5xl md:text-7xl lg:text-8xl font-bold mb-6 opacity-0 animate-fade-in" style={{ animationDelay: '0.1s' }}>
          <span className="text-gradient">Bold</span>
          <span className="text-foreground"> for Delphi</span>
        </h1>

        {/* Subtitle */}
        <p className="text-xl md:text-2xl text-muted-foreground mb-4 max-w-3xl mx-auto opacity-0 animate-fade-in" style={{ animationDelay: '0.2s' }}>
          Model-Driven Architecture & ORM Framework
        </p>

        <p className="text-lg text-muted-foreground/80 mb-10 max-w-2xl mx-auto opacity-0 animate-fade-in" style={{ animationDelay: '0.3s' }}>
          Build enterprise applications faster with UML modeling, automatic code generation, 
          and seamless database persistence. Define your model, let Bold do the rest.
        </p>

        {/* CTA Buttons */}
        <div className="flex flex-col sm:flex-row gap-4 justify-center items-center opacity-0 animate-fade-in" style={{ animationDelay: '0.4s' }}>
          <Button variant="hero" size="xl" asChild>
            <a href="#demos">
              Explore Demos
              <ArrowDown className="w-5 h-5 ml-1" />
            </a>
          </Button>
          <Button variant="glass" size="lg" asChild>
            <a href="https://github.com/Embarcadero/BoldForDelphi" target="_blank" rel="noopener noreferrer">
              <Github className="w-5 h-5 mr-2" />
              View on GitHub
            </a>
          </Button>
          <Button variant="outline" size="lg" asChild>
            <a href="#docs">
              <BookOpen className="w-4 h-4 mr-2" />
              Documentation
            </a>
          </Button>
        </div>

        {/* Stats */}
        <div className="grid grid-cols-3 gap-8 mt-16 pt-16 border-t border-border/50 opacity-0 animate-fade-in" style={{ animationDelay: '0.5s' }}>
          <div className="text-center">
            <div className="text-3xl md:text-4xl font-bold text-gradient">155+</div>
            <div className="text-sm text-muted-foreground mt-1">GitHub Stars</div>
          </div>
          <div className="text-center">
            <div className="text-3xl md:text-4xl font-bold text-gradient">58+</div>
            <div className="text-sm text-muted-foreground mt-1">Forks</div>
          </div>
          <div className="text-center">
            <div className="text-3xl md:text-4xl font-bold text-gradient">20+</div>
            <div className="text-sm text-muted-foreground mt-1">Years Active</div>
          </div>
        </div>
      </div>

      {/* Scroll indicator */}
      <div className="absolute bottom-8 left-1/2 -translate-x-1/2 opacity-0 animate-fade-in" style={{ animationDelay: '0.6s' }}>
        <div className="w-6 h-10 rounded-full border-2 border-muted-foreground/30 flex items-start justify-center p-1.5">
          <div className="w-1.5 h-3 bg-primary rounded-full animate-bounce" />
        </div>
      </div>
    </section>
  );
}
