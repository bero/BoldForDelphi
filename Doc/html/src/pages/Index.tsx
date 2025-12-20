import { Header, Footer } from "@/components/Layout";
import { HeroSection } from "@/components/HeroSection";
import { FeaturesSection } from "@/components/FeaturesSection";
import { DemosSection } from "@/components/DemosSection";
import { DocumentationSection } from "@/components/DocumentationSection";

const Index = () => {
  return (
    <div className="min-h-screen bg-background">
      <Header />
      <main>
        <HeroSection />
        <FeaturesSection />
        <DemosSection />
        <DocumentationSection />
      </main>
      <Footer />
    </div>
  );
};

export default Index;
