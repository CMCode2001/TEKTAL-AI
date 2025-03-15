import { HomeIcon, GraduationCap, BookOpen, School, Book, Map, MessageCircle } from 'lucide-react';
import { Link, useLocation, useNavigate } from 'react-router-dom';

interface ResultatOrientation {
  etablissements_recommandes: string[];
  matieres_requises: string[];
  message: string;
  parcours_possibles: string[];
}

interface Utilisateur {
  nom: string;
  niveau_etude: string;
  filiere: string;
  domaine_interet: string;
}

function ResultatOrientation() {
  const navigate = useNavigate();
  const location = useLocation();

  // Récupérer les données passées via l'état de navigation
  const { idUtilisateur, utilisateurData, resultatsAnalyse } = location.state || {};

  // Log pour vérifier les données reçues
  console.log("Données reçues dans resultatOrientation :", {
    idUtilisateur,
    utilisateurData,
    resultatsAnalyse,
  });

  // Vérifier si les données sont disponibles
  if (!idUtilisateur || !utilisateurData || !resultatsAnalyse) {
    return (
      <div className="flex flex-col items-center justify-center min-h-screen bgCMC p-10">
        <p className="text-red-500 text-lg font-medium">Données manquantes. Veuillez recommencer le processus.</p>
        <button onClick={() => navigate('/')} className="mt-4 px-4 py-2 bg-green-600 text-white rounded-md hover:bg-green-700 transition-colors">
          Retour à l'accueil
        </button>
      </div>
    );
  }

  const utilisateur: Utilisateur = utilisateurData;
  const resultats: ResultatOrientation = resultatsAnalyse;

  return (
    <div className="min-h-screen bgCMC p-10">
      <Link to="/" className="absolute top-4 left-4 flex items-center text-white hover:text-yellow-400 transition-colors">
        <HomeIcon className="w-6 h-6 mr-2" />
        Accueil
      </Link>
      <div className="max-w-6xl mx-auto space-y-8">
        {/* Section 1 : Carte de bienvenue */}
        <div className="bg-white rounded-xl shadow-lg p-8 hover:shadow-xl transition-shadow">
          <h1 className="text-2xl font-bold text-gray-800 mb-4">👋 Bonjour, {utilisateur.nom} !</h1>
          <p className="text-gray-600 mb-4">Voici un résumé des données que vous avez fournies :</p>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <p className="text-gray-800 flex items-center">
                <GraduationCap className="w-5 h-5 mr-2 text-blue-500" />
                <span className="font-semibold">Niveau d'étude :</span> {utilisateur.niveau_etude}
              </p>
              <p className="text-gray-800 flex items-center">
                <BookOpen className="w-5 h-5 mr-2 text-green-500" />
                <span className="font-semibold">Filière :</span> {utilisateur.filiere}
              </p>
              <p className="text-gray-800 flex items-center">
                <Map className="w-5 h-5 mr-2 text-purple-500" />
                <span className="font-semibold">Domaine d'intérêt :</span> {utilisateur.domaine_interet}
              </p>
            </div>
          </div>
        </div>

        {/* Section 2 : Statistiques sur l'orientation */}
        <div className="bg-white rounded-xl shadow-lg p-8 hover:shadow-xl transition-shadow">
          <h2 className="text-2xl font-bold text-gray-800 mb-6">🎯 Statistiques sur mon orientation</h2>

          {/* Établissements recommandés */}
          <div className="mb-8">
            <h3 className="text-xl font-semibold text-gray-800 mb-4 flex items-center">
              <School className="w-6 h-6 mr-2 text-blue-500" />
              Établissements recommandés
            </h3>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              {resultats.etablissements_recommandes.map((etablissement, index) => (
                <div key={index} className="bg-green-50 p-4 rounded-lg hover:bg-green-100 transition-colors">
                  <p className="text-green-800 font-medium flex items-center">
                    <School className="w-5 h-5 mr-2 text-green-500" />
                    {etablissement}
                  </p>
                </div>
              ))}
            </div>
          </div>

          {/* Matières requises */}
          <div className="mb-8">
            <h3 className="text-xl font-semibold text-gray-800 mb-4 flex items-center">
              <Book className="w-6 h-6 mr-2 text-blue-500" />
              Matières requises
            </h3>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              {resultats.matieres_requises.map((matiere, index) => (
                <div key={index} className="bg-blue-50 p-4 rounded-lg hover:bg-blue-100 transition-colors">
                  <p className="text-blue-800 font-medium flex items-center">
                    <Book className="w-5 h-5 mr-2 text-blue-500" />
                    {matiere}
                  </p>
                </div>
              ))}
            </div>
          </div>

          {/* Parcours possibles */}
          <div className="mb-8">
            <h3 className="text-xl font-semibold text-gray-800 mb-4 flex items-center">
              <Map className="w-6 h-6 mr-2 text-purple-500" />
              Parcours possibles
            </h3>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              {resultats.parcours_possibles.map((parcours, index) => (
                <div key={index} className="bg-purple-50 p-4 rounded-lg hover:bg-purple-100 transition-colors">
                  <p className="text-purple-800 font-medium flex items-center">
                    <Map className="w-5 h-5 mr-2 text-purple-500" />
                    {parcours}
                  </p>
                </div>
              ))}
            </div>
          </div>

          {/* Message de recommandation */}
          <div className="bg-yellow-50 p-6 rounded-lg hover:bg-yellow-100 transition-colors">
            <h3 className="text-xl font-semibold text-yellow-800 mb-4 flex items-center">
              <MessageCircle className="w-6 h-6 mr-2 text-yellow-500" />
              Message de recommandation
            </h3>
            <p className="text-yellow-800">{resultats.message}</p>
          </div>
        </div>
      </div>
    </div>
  );
}

export default ResultatOrientation;