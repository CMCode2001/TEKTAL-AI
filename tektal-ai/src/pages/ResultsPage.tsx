import { useLocation, useNavigate } from 'react-router-dom';
import { ArrowLeft, Briefcase, GraduationCap, BookOpen } from 'lucide-react';
import Logo from "../assets/LogoTkT.png"


interface ResultsState {
  userInfo: {
    firstName: string;
    lastName: string;
  };
  answers: number[];
  questions: unknown[];
}

const careerSuggestions = {
  tech: {
    careers: ['Développeur Full-Stack', 'Architecte Cloud', 'Data Scientist', 'DevOps Engineer'],
    skills: ['Programmation', 'Algorithmes', 'Base de données', 'Architecture système'],
    courses: ['Computer Science', 'Software Engineering', 'Data Analytics']
  },
  science: {
    careers: ['Ingénieur R&D', 'Chercheur', 'Ingénieur Biomédical', 'Analyste Quantitatif'],
    skills: ['Mathématiques', 'Physique', 'Méthode scientifique', 'Analyse de données'],
    courses: ['Sciences Appliquées', 'Ingénierie', 'Mathématiques']
  },
  business: {
    careers: ['Chef de Projet', 'Consultant en Stratégie', 'Analyste Financier', 'Entrepreneur'],
    skills: ['Gestion', 'Finance', 'Marketing', 'Leadership'],
    courses: ['Business Administration', 'Finance', 'Marketing']
  },
  creative: {
    careers: ['Designer UX/UI', 'Directeur Artistique', 'Product Designer', 'Motion Designer'],
    skills: ['Design', 'Créativité', 'Communication visuelle', 'User Experience'],
    courses: ['Design', 'Arts Numériques', 'Communication Visuelle']
  }
};

function ResultsPage() {
  const navigate = useNavigate();
  const location = useLocation();
  const { userInfo, answers } = location.state as ResultsState;

  // Simple logic to determine career path based on answers
  const getDomainFromAnswers = () => {
    const interestAnswer = answers[1]; // Domain interest question
    switch (interestAnswer) {
      case 0: return 'tech';
      case 1: return 'science';
      case 2: return 'business';
      case 3: return 'creative';
      default: return 'tech';
    }
  };

  const domain = getDomainFromAnswers();
  const suggestions = careerSuggestions[domain];

  return (
    <div className="min-h-screen bgCMC text-white">
      <nav className="px-6 py-4">
        <div className="container mx-auto flex justify-between items-center">
          <button
            onClick={() => navigate('/')}
            className="text-gray-400 hover:text-white transition-colors flex items-center gap-2"
          >
            <ArrowLeft className="w-5 h-5" />
            Retour
          </button>
          <div className="flex items-center space-x-2">
            {/* <Brain className="h-8 w-8 text-blue-400" /> */}
            <img src={Logo} alt="Logo" width={50} />
            <span className="text-white text-2xl font-bold">TEKTAL-<span className='text-yellow-400'>AI</span></span>
          </div>
        </div>
      </nav>

      <main className="container mx-auto px-6 py-12 max-w-4xl">
        <div className="bg-gray-800 rounded-xl p-8 mb-8">
          <h1 className="text-3xl font-bold mb-4">
            Bonjour {userInfo.firstName} {userInfo.lastName}
          </h1>
          <p className="text-gray-300 text-lg">
            Basé sur vos réponses, nous avons analysé votre profil et préparé des recommandations personnalisées.
          </p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
          <div className="bg-gray-800 rounded-xl p-6">
            <div className="flex items-center gap-3 mb-4">
              <Briefcase className="w-6 h-6 text-yellow-400" />
              <h2 className="text-xl font-semibold">Carrières Recommandées</h2>
            </div>
            <ul className="space-y-3">
              {suggestions.careers.map((career, index) => (
                <li key={index} className="flex items-center gap-2">
                  <span className="w-2 h-2 bg-yellow-400 rounded-full"></span>
                  <span>{career}</span>
                </li>
              ))}
            </ul>
          </div>

          <div className="bg-gray-800 rounded-xl p-6">
            <div className="flex items-center gap-3 mb-4">
              <GraduationCap className="w-6 h-6 text-yellow-400" />
              <h2 className="text-xl font-semibold">Formations Recommandées</h2>
            </div>
            <ul className="space-y-3">
              {suggestions.courses.map((course, index) => (
                <li key={index} className="flex items-center gap-2">
                  <span className="w-2 h-2 bg-yellow-400 rounded-full"></span>
                  <span>{course}</span>
                </li>
              ))}
            </ul>
          </div>
        </div>

        <div className="mt-8 bg-gray-800 rounded-xl p-6">
          <div className="flex items-center gap-3 mb-4">
            <BookOpen className="w-6 h-6 text-green-400" />
            <h2 className="text-xl font-semibold">Compétences Clés à Développer</h2>
          </div>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            {suggestions.skills.map((skill, index) => (
              <div key={index} className="bg-gray-700 rounded-lg p-3 text-center">
                {skill}
              </div>
            ))}
          </div>
        </div>
      </main>
    </div>
  );
}

export default ResultsPage;