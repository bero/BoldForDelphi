import { Code2, Layers, Zap, Database } from "lucide-react";

const features = [
  {
    icon: Code2,
    title: "Model-Driven Architecture",
    description: "Define your business model visually with UML and let Bold generate the code automatically.",
  },
  {
    icon: Database,
    title: "Object-Relational Mapping",
    description: "Seamless persistence layer that maps your objects to any database without manual SQL.",
  },
  {
    icon: Layers,
    title: "Business Logic Layer",
    description: "Encapsulate complex business rules in derived attributes and constraints.",
  },
  {
    icon: Zap,
    title: "High Performance",
    description: "Optimized caching and lazy loading ensure your applications run blazingly fast.",
  },
];

export function FeaturesSection() {
  return (
    <section className="py-24 px-6">
      <div className="max-w-6xl mx-auto">
        <div className="text-center mb-16">
          <h2 className="text-3xl md:text-4xl font-bold mb-4">
            Why Choose <span className="text-gradient">Bold</span>?
          </h2>
          <p className="text-muted-foreground text-lg max-w-2xl mx-auto">
            Bold for Delphi revolutionizes how you build enterprise applications with its powerful MDA approach.
          </p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
          {features.map((feature, index) => (
            <div
              key={feature.title}
              className="group p-6 rounded-xl bg-card border border-border hover:border-primary/50 transition-all duration-300 hover-lift glow-border opacity-0 animate-fade-in"
              style={{ animationDelay: `${index * 0.1}s` }}
            >
              <div className="w-12 h-12 rounded-lg bg-primary/10 flex items-center justify-center mb-4 group-hover:bg-primary/20 transition-colors">
                <feature.icon className="w-6 h-6 text-primary" />
              </div>
              <h3 className="text-lg font-semibold mb-2">{feature.title}</h3>
              <p className="text-muted-foreground text-sm">{feature.description}</p>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}
