import { Link } from 'react-router-dom';
import { GraduationCap, Briefcase, ArrowLeft } from 'lucide-react';

function MonGuide() {
  return (
    <div className="min-h-screen bgCMC flex items-center justify-center p-4 relative">
      {/* Bouton Précédent */}
      <Link to="/" className="absolute top-4 left-4 flex items-center text-white hover:text-yellow-400 transition-colors">
        <ArrowLeft className="w-6 h-6 mr-2" />
        Précédent
      </Link>
      
      <div className="max-w-4xl w-full">
        <h1 className="text-5xl font-bold text-center text-white mb-8">
          TEKTAL <span className='text-yellow-400'>AI</span> - Système d'Orientation
        </h1>
        <br/>
        <div className="grid md:grid-cols-2 gap-8">
          <Link
            to="/monguide-academique"
            className="bg-white rounded-xl shadow-lg p-8 hover:shadow-xl transition-shadow duration-300"
          >
            <div className="flex flex-col items-center text-center">
              <GraduationCap className="w-16 h-16 text-yellow-400 mb-4" />
              <h2 className="text-2xl font-bold text-gray-800">
                Orientation Académique
              </h2>
              <p className="text-gray-600">
                Découvrez les domaines d'études qui correspondent à votre profil et obtenez des recommandations personnalisées.
              </p>
            </div>
          </Link>

          <Link
            to="/monguide-professionnel"
            className="bg-white rounded-xl shadow-lg p-8 hover:shadow-xl transition-shadow duration-300"
          >
            <div className="flex flex-col items-center text-center">
              <Briefcase className="w-16 h-16 text-green-400 mb-4" />
              <h2 className="text-2xl font-bold text-gray-800 mb-2">
                Orientation Professionnelle
              </h2>
              <p className="text-gray-600">
                Explorez les métiers et trouvez votre voie professionnelle en fonction de vos compétences et personnalité.
              </p>
            </div>
          </Link>
        </div>
      </div>
    </div>
  );
}

export default MonGuide;
