import { Card, CardHeader, CardTitle, CardDescription, CardContent, CardFooter } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { ExternalLink, FileCode, Database, Users, ShoppingCart, Building2, BookOpen, Truck, Settings, Layers } from "lucide-react";

const demos = [
  {
    id: "building-example",
    title: "Building Example",
    description: "Comprehensive demo showing building management with floors, rooms, and tenants. Demonstrates master-detail relationships and derived attributes.",
    icon: Building2,
    category: "Enterprise",
    features: ["Master-Detail Views", "Derived Attributes", "Complex Relationships"],
  },
  {
    id: "person-example",
    title: "Person Example",
    description: "Classic person/address model demonstrating inheritance hierarchies, associations, and the Bold constraint system.",
    icon: Users,
    category: "Tutorial",
    features: ["Inheritance", "Associations", "Constraints"],
  },
  {
    id: "product-catalog",
    title: "Product Catalog",
    description: "E-commerce style product management with categories, pricing rules, and inventory tracking using Bold's OCL expressions.",
    icon: ShoppingCart,
    category: "E-Commerce",
    features: ["OCL Expressions", "Business Rules", "Dynamic Pricing"],
  },
  {
    id: "document-manager",
    title: "Document Manager",
    description: "File and document management system showcasing Bold's blob handling, versioning, and full-text search capabilities.",
    icon: FileCode,
    category: "Utility",
    features: ["Blob Storage", "Versioning", "Search"],
  },
  {
    id: "order-system",
    title: "Order System",
    description: "Complete order processing workflow with customers, orders, and line items. Demonstrates transaction handling and validation.",
    icon: Truck,
    category: "Enterprise",
    features: ["Transactions", "Validation", "Workflow"],
  },
  {
    id: "model-editor",
    title: "Model Editor",
    description: "Visual UML model editor demonstrating Bold's meta-model capabilities and code generation features.",
    icon: Settings,
    category: "Tools",
    features: ["UML Modeling", "Code Generation", "Meta-Model"],
  },
  {
    id: "asp-integration",
    title: "ASP Integration",
    description: "Web application example showing Bold integration with Active Server Pages for web-based data entry and reporting.",
    icon: Layers,
    category: "Web",
    features: ["Web Forms", "Data Binding", "Reports"],
  },
  {
    id: "getting-started",
    title: "Getting Started",
    description: "Step-by-step tutorial application that guides you through Bold's core concepts with hands-on examples.",
    icon: BookOpen,
    category: "Tutorial",
    features: ["Beginner Friendly", "Interactive", "Documentation"],
  },
];

const categoryColors: Record<string, string> = {
  Enterprise: "bg-primary/20 text-primary",
  Tutorial: "bg-accent/20 text-accent",
  "E-Commerce": "bg-green-500/20 text-green-400",
  Utility: "bg-amber-500/20 text-amber-400",
  Tools: "bg-rose-500/20 text-rose-400",
  Web: "bg-blue-500/20 text-blue-400",
};

export function DemosSection() {
  return (
    <section id="demos" className="py-24 px-6 bg-secondary/30">
      <div className="max-w-7xl mx-auto">
        <div className="text-center mb-16">
          <span className="inline-block px-4 py-1.5 rounded-full text-sm font-medium bg-primary/10 text-primary border border-primary/20 mb-4">
            Demo Applications
          </span>
          <h2 className="text-3xl md:text-4xl font-bold mb-4">
            Explore <span className="text-gradient">Working Examples</span>
          </h2>
          <p className="text-muted-foreground text-lg max-w-2xl mx-auto">
            Dive into our collection of demo applications showcasing Bold's capabilities in real-world scenarios.
          </p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-6">
          {demos.map((demo, index) => (
            <Card
              key={demo.id}
              variant="glow"
              className="flex flex-col opacity-0 animate-fade-in"
              style={{ animationDelay: `${index * 0.1}s` }}
            >
              <CardHeader>
                <div className="flex items-start justify-between mb-2">
                  <div className="w-10 h-10 rounded-lg bg-primary/10 flex items-center justify-center">
                    <demo.icon className="w-5 h-5 text-primary" />
                  </div>
                  <span className={`text-xs font-medium px-2 py-1 rounded-full ${categoryColors[demo.category]}`}>
                    {demo.category}
                  </span>
                </div>
                <CardTitle className="text-lg">{demo.title}</CardTitle>
                <CardDescription className="line-clamp-3">
                  {demo.description}
                </CardDescription>
              </CardHeader>
              <CardContent className="flex-1">
                <div className="flex flex-wrap gap-1.5">
                  {demo.features.map((feature) => (
                    <span
                      key={feature}
                      className="text-xs px-2 py-0.5 rounded-md bg-muted text-muted-foreground"
                    >
                      {feature}
                    </span>
                  ))}
                </div>
              </CardContent>
              <CardFooter>
                <Button variant="glow" size="sm" className="w-full group">
                  View Demo
                  <ExternalLink className="w-3.5 h-3.5 ml-1 group-hover:translate-x-0.5 transition-transform" />
                </Button>
              </CardFooter>
            </Card>
          ))}
        </div>
      </div>
    </section>
  );
}
