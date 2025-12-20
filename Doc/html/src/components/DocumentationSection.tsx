import { Card, CardContent } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { BookOpen, FileText, Video, Code, ArrowRight } from "lucide-react";

const resources = [
  {
    icon: BookOpen,
    title: "Getting Started Guide",
    description: "Learn the fundamentals of Bold for Delphi with our comprehensive beginner's guide.",
    link: "#",
    color: "text-primary",
  },
  {
    icon: FileText,
    title: "API Reference",
    description: "Complete documentation of all Bold classes, methods, and properties.",
    link: "#",
    color: "text-accent",
  },
  {
    icon: Video,
    title: "Video Tutorials",
    description: "Watch step-by-step video tutorials covering common use cases and patterns.",
    link: "#",
    color: "text-green-400",
  },
  {
    icon: Code,
    title: "Code Examples",
    description: "Browse through annotated code samples demonstrating best practices.",
    link: "#",
    color: "text-amber-400",
  },
];

export function DocumentationSection() {
  return (
    <section id="docs" className="py-24 px-6">
      <div className="max-w-6xl mx-auto">
        <div className="text-center mb-16">
          <h2 className="text-3xl md:text-4xl font-bold mb-4">
            Learn & <span className="text-gradient">Master</span>
          </h2>
          <p className="text-muted-foreground text-lg max-w-2xl mx-auto">
            Comprehensive resources to help you get the most out of Bold for Delphi.
          </p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          {resources.map((resource, index) => (
            <Card
              key={resource.title}
              variant="elevated"
              className="group cursor-pointer opacity-0 animate-fade-in"
              style={{ animationDelay: `${index * 0.1}s` }}
            >
              <CardContent className="p-6 flex items-start gap-4">
                <div className={`w-12 h-12 rounded-xl bg-secondary flex items-center justify-center flex-shrink-0 group-hover:scale-110 transition-transform ${resource.color}`}>
                  <resource.icon className="w-6 h-6" />
                </div>
                <div className="flex-1">
                  <h3 className="font-semibold text-lg mb-1 group-hover:text-primary transition-colors">
                    {resource.title}
                  </h3>
                  <p className="text-muted-foreground text-sm mb-3">
                    {resource.description}
                  </p>
                  <span className="text-sm text-primary flex items-center gap-1 group-hover:gap-2 transition-all">
                    Learn more <ArrowRight className="w-4 h-4" />
                  </span>
                </div>
              </CardContent>
            </Card>
          ))}
        </div>

        {/* CTA Section */}
        <div className="mt-16 text-center p-8 rounded-2xl glass-effect border border-border/50">
          <h3 className="text-2xl font-bold mb-3">Ready to Build Something Amazing?</h3>
          <p className="text-muted-foreground mb-6 max-w-xl mx-auto">
            Join thousands of Delphi developers who use Bold to create powerful, 
            maintainable enterprise applications.
          </p>
          <div className="flex flex-col sm:flex-row gap-4 justify-center">
            <Button variant="hero" size="lg" asChild>
              <a href="https://github.com/Embarcadero/BoldForDelphi" target="_blank" rel="noopener noreferrer">
                Download Now
              </a>
            </Button>
            <Button variant="outline" size="lg">
              View Source Code
            </Button>
          </div>
        </div>
      </div>
    </section>
  );
}
