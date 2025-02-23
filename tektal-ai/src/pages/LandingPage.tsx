import {  ArrowRight } from 'lucide-react';
import { useNavigate } from 'react-router-dom';
import Logo from "../assets/LogoTkT.png"
import ImgCMC from "../assets/ok.jpg"

function LandingPage() {
  const navigate = useNavigate();

  return (
    <div className="min-h-screen bgCMC">
      {/* Navigation */}
      <nav className="px-6 py-4">
        <div className="container mx-auto flex justify-between items-center">
          <div className="flex items-center ">
            <img src={Logo} alt="Logo" width={100} />
            <span className="text-white text-2xl font-bold">TEKTAL- <span className='text-yellow-400'>AI</span></span>
          </div>
          
        </div>
      </nav>

      {/* Hero Section */}
      <main className="container mx-auto px-6 py-10 flex flex-col lg:flex-row items-center">
        <div className="lg:w-1/2 lg:pr-12">
          <h1 className="text-5xl lg:text-7xl font-bold text-white mb-6">
            TEKTAL-<span className='text-yellow-400'>AI</span>
          </h1>
          <p className="text-xl lg:text-2xl text-gray-300 mb-8 leading-relaxed">
            Une plateforme d'orientation intelligente qui vous guide vers votre 
            parcours académique et professionnel idéal grâce à un systeme expert.
          </p>
          
          <button 
            onClick={() => navigate('/questionnaire')}
            className="bg-green-900 hover:bg-green-700 text-white px-8 py-4 rounded-lg 
              text-lg font-semibold flex items-center space-x-2 transition-colors"
          >
            <span>Découvrez votre voie</span>
            <ArrowRight className="w-5 h-5" />
          </button>

          <div className="mt-12 grid grid-cols-1 sm:grid-cols-2 gap-6">
            <div className="bg-white/10 rounded-lg p-4 backdrop-blur-sm">
              <h3 className="text-green-400 font-semibold mb-2"><span className='text-white'>✦</span>  Systeme Expert</h3>
              <p className="text-gray-300">Analyse personnalisée basée sur vos compétences et aspirations</p>
            </div>
            <div className="bg-white/10 rounded-lg p-4 backdrop-blur-sm">
              <h3 className="text-green-400 font-semibold mb-2"> <span className='text-white'>✦</span> Expertise</h3>
              <p className="text-gray-300">Recommandations basées sur des données du marché du travail</p>
            </div>
          </div>
        </div>

        <div className="lg:w-1/2 mt-12 lg:mt-0">
          <div className="relative">
            <div className="absolute -inset-4 bg-green-500/20 rounded-3xl blur-xl"></div>
            <div className="relative bg-gradient-to-br from-black to-black-900 rounded-3xl p-4 ">
              <img 
                src={ImgCMC}
                alt="Professional Guidance Interface"
                className="rounded-2xl w-full shadow-2xl"
              />
            </div>
          </div>
        </div>
      </main>
    </div>
  );
}

export default LandingPage