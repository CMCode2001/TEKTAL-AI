import { Link } from 'react-router-dom';
import { Search, Compass, ArrowLeft } from 'lucide-react';
import Logo from '../../assets/LogoTkT.png'
function ProfessionnelleOrientation() {
  return (
    <div className="min-h-screen p-8">
      <Link to="/monguide-professionnel" className="absolute top-4 left-4 flex items-center text-white hover:text-yellow-400 transition-colors">
        <ArrowLeft className="w-6 h-6 mr-2" />
        Précédent
      </Link>
      
      <div className="flex justify-center items-center">
        <img src={Logo} alt="Logo" width={200} />
      </div>
      <div className="max-w-4xl mx-auto"></div>
      <div className="max-w-4xl mx-auto">
        <h1 className="text-3xl font-bold text-gray-800 mb-8 text-center">
          Orientation Professionnelle
        </h1>
        
        <div className="grid md:grid-cols-2 gap-8">
          <Link
            to="/professional/explore"
            className="bg-white rounded-xl shadow-lg p-8 hover:shadow-xl transition-shadow duration-300"
          >
            <div className="flex flex-col items-center text-center">
              <Search className="w-12 h-12 text-blue-600 mb-4" />
              <h2 className="text-xl font-semibold text-gray-800 mb-2">
                Explorer les Métiers
              </h2>
              <p className="text-gray-600">
                Découvrez tous les métiers disponibles et leurs spécificités.
              </p>
            </div>
          </Link>

          <Link
            to="/professional/orient"
            className="bg-white rounded-xl shadow-lg p-8 hover:shadow-xl transition-shadow duration-300"
          >
            <div className="flex flex-col items-center text-center">
              <Compass className="w-12 h-12 text-indigo-600 mb-4" />
              <h2 className="text-xl font-semibold text-gray-800 mb-2">
                Trouver un Métier
              </h2>
              <p className="text-gray-600">
                Obtenez des recommandations personnalisées basées sur votre profil et vos compétences.
              </p>
            </div>
          </Link>
        </div>

        <Link
          to="/"
          className="block mt-8 text-center text-blue-600 hover:text-blue-800"
        >
          Retour à l'accueil
        </Link>
      </div>
    </div>
  );
}

export default ProfessionnelleOrientation;