import { HomeIcon, GraduationCap, BookOpen, Wrench, Brain, School, Lightbulb } from 'lucide-react';
import { Link, useLocation, useNavigate } from 'react-router-dom';

interface ResultatOrientationPro {
  domaines_suggeres: string[]; // Domaines suggérés par le serveur
  message: string; // Message de recommandation
}

interface UtilisateurPro {
  nom: string;
  niveau_etude: string;
  filiere: string;
  competences: string[];
  traits_personnalite: string[];
}

function ResultatOrientationPro() {
  const navigate = useNavigate();
  const location = useLocation();

  // Récupérer les données passées via l'état de navigation
  const { resultats, utilisateurData } = location.state || {};

  // Log pour vérifier les données reçues
  console.log("Données reçues dans ResultatOrientationPro :", { resultats, utilisateurData });

  // Vérifier si les données sont disponibles
  if (!resultats || !utilisateurData) {
    return (
      <div className="flex flex-col items-center justify-center min-h-screen bgCMC p-10">
        <p className="text-red-500 text-lg font-medium">Données manquantes. Veuillez recommencer le processus.</p>
        <button onClick={() => navigate('/')} className="mt-4 px-4 py-2 bg-green-600 text-white rounded-md hover:bg-green-700 transition-colors">
          Retour à l'accueil
        </button>
      </div>
    );
  }

  return (
    <div className="min-h-screen bgCMC p-10">
      <Link to="/" className="absolute top-4 left-4 flex items-center text-white hover:text-yellow-400 transition-colors">
        <HomeIcon className="w-6 h-6 mr-2" />
        Accueil
      </Link>
      <br />
      <div className="max-w-6xl mx-auto space-y-8">
        {/* Section 1 : Carte de bienvenue */}
        <div className="bg-white rounded-xl shadow-lg p-8 hover:shadow-xl transition-shadow">
          <h1 className="text-2xl font-bold text-gray-800 mb-4">👋 Bonjour, {utilisateurData.nom} !</h1>
          <p className="text-gray-600 mb-4">Voici un résumé des données que vous avez fournies :</p>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <p className="text-gray-800 flex items-center">
                <GraduationCap className="w-5 h-5 mr-2 text-blue-500" />
                <span className="font-semibold">Niveau d'étude :</span> {utilisateurData.niveau_etude}
              </p>
              <p className="text-gray-800 flex items-center">
                <BookOpen className="w-5 h-5 mr-2 text-green-500" />
                <span className="font-semibold">Filière :</span> {utilisateurData.filiere}
              </p>
              <p className="text-gray-800 flex items-center">
                <Wrench className="w-5 h-5 mr-2 text-purple-500" />
                <span className="font-semibold">Compétences :</span> {utilisateurData.competences.join(', ')}
              </p>
              <p className="text-gray-800 flex items-center">
                <Brain className="w-5 h-5 mr-2 text-yellow-500" />
                <span className="font-semibold">Traits de personnalité :</span> {utilisateurData.traits_personnalite.join(', ')}
              </p>
            </div>
          </div>
        </div>

        {/* Section 2 : Résultats de l'orientation */}
        <div className="bg-white rounded-xl shadow-lg p-8 hover:shadow-xl transition-shadow">
          <h2 className="text-2xl font-bold text-gray-800 mb-6">🎯 Résultats de votre orientation professionnelle</h2>

          {/* Domaines suggérés */}
          {resultats.domaines_suggeres && resultats.domaines_suggeres.length > 0 && (
            <div className="mb-8">
              <h3 className="text-xl font-semibold text-gray-800 mb-4 flex items-center">
                <School className="w-6 h-6 mr-2 text-blue-500" />
                Domaines suggérés
              </h3>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                {resultats.domaines_suggeres.map((monDomaine, k) => (
                  <div key={k} className="bg-green-50 p-4 rounded-lg hover:bg-green-100 transition-colors">
                    <p className="text-green-800 font-medium flex items-center">
                      <Lightbulb className="w-5 h-5 mr-2 text-green-500" />
                      {monDomaine}
                    </p>
                  </div>
                ))}
              </div>
            </div>
          )}

          {/* Message de recommandation */}
          {resultats.message && (
            <div className="bg-yellow-50 p-6 rounded-lg hover:bg-yellow-100 transition-colors">
              <h3 className="text-xl font-semibold text-yellow-800 mb-4 flex items-center">
                💡 Message de recommandation
              </h3>
              <p className="text-yellow-800">{resultats.message}</p>
            </div>
          )}
        </div>
      </div>
    </div>
  );
}

export default ResultatOrientationPro;