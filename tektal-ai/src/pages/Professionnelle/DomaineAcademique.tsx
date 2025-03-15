import { Link } from 'react-router-dom';
import { Code, Stethoscope, Scale, Building2, Pencil, ArrowLeft, Trash, PartyPopper, Factory, HandHelpingIcon, Microscope } from 'lucide-react';

function DomaineAcademique() {
  return (
    <div className="min-h-screen p-8 bgCMC">
         {/* Bouton Précédent */}
      <Link to="/monguide-academique" className="absolute top-4 left-4 flex items-center text-white hover:text-yellow-400 transition-colors">
        <ArrowLeft className="w-6 h-6 mr-2" />
        Précédent
      </Link>
      <div className="max-w-4xl mx-auto">
        <h1 className="text-5xl font-bold text-green-400 mb-8 text-center">
          Explorer les Métiers
        </h1>

        <div className="grid md:grid-cols-2 lg:grid-cols-3 gap-6">
          <div className="bg-white rounded-xl shadow-lg p-6">
            <div className="flex flex-col items-center text-center">
              <Code className="w-12 h-12 text-blue-600 mb-4" />
              <h2 className="text-xl font-semibold text-gray-800 mb-2">Informatique</h2>
              <p className="text-gray-600">
                Développeur, Architecte logiciel, Data Scientist
              </p>
            </div>
          </div>

          <div className="bg-white rounded-xl shadow-lg p-6">
            <div className="flex flex-col items-center text-center">
              <Stethoscope className="w-12 h-12 text-red-600 mb-4" />
              <h2 className="text-xl font-semibold text-gray-800 mb-2">Santé</h2>
              <p className="text-gray-600">
                Médecin, Infirmier, Pharmacien
              </p>
            </div>
          </div>

          <div className="bg-white rounded-xl shadow-lg p-6">
            <div className="flex flex-col items-center text-center">
              <Scale className="w-12 h-12 text-indigo-600 mb-4" />
              <h2 className="text-xl font-semibold text-gray-800 mb-2">Sciences Humaines</h2>
              <p className="text-gray-600">
                Gestionnaire d'entreprise, Sociologue, Economiste
              </p>
            </div>
          </div>

          <div className="bg-white rounded-xl shadow-lg p-6">
            <div className="flex flex-col items-center text-center">
              <HandHelpingIcon className="w-12 h-12 text-green-600 mb-4" />
              <h2 className="text-xl font-semibold text-gray-800 mb-2">Commerce</h2>
              <p className="text-gray-600">
                Manager, Consultant, Entrepreneur
              </p>
            </div>
          </div>

          <div className="bg-white rounded-xl shadow-lg p-6">
            <div className="flex flex-col items-center text-center">
              <Microscope className="w-12 h-12 text-purple-600 mb-4" />
              <h2 className="text-xl font-semibold text-gray-800 mb-2">Ingénierie</h2>
              <p className="text-gray-600">
                Designer, Architecte, Artiste
              </p>
            </div>
          </div>
          <div className="bg-white rounded-xl shadow-lg p-6">
            <div className="flex flex-col items-center text-center">
              <Pencil className="w-12 h-12 text-gray-600 mb-4" />
              <h2 className="text-xl font-semibold text-gray-800 mb-2">Création</h2>
              <p className="text-gray-600">
                Designer, Architecte, Artiste
              </p>
            </div>
          </div>
          </div>

        
      </div>
    </div>
  );
}

export default DomaineAcademique;