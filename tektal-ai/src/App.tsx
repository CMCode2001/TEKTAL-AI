import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import LandingPage from './pages/LandingPage';
import QuestionnairePage from './pages/QuestionnairePage';
import ResultsPage from './pages/ResultsPage';
import MonGuide from './pages/MonGuide';
import AcademicOrientation from './pages/Academique/AcademicOrientation';
import DomaineAcademique from './pages/Academique/DomaineAcademique';
import FormulaireOrientation from './pages/Academique/FormulaireOrientation';
import ResultatOrientation from './pages/Academique/ResultatOrientation';
import ProfessionnelleOrientation from './pages/Professionnelle/ProfessionnelleOrientation';
import DomaineProfessionnelle from './pages/Professionnelle/DomaineProfessionnelle';
import FormulaireOrientationPro from './pages/Professionnelle/FormulaireOrientationPro';
import ResultatOrientationPro from './pages/Professionnelle/ResultatOrientationPro';

function App() {
  return (
    <Router>
      <Routes>
        <Route path="/" element={<LandingPage />} />
        <Route path="/monguide" element={<MonGuide />} />
        {/*----- ACADEMIQUE ------*/}
        <Route path="/monguide-academique" element={<AcademicOrientation />} />
        <Route path="/monguide-domaine-academique" element={<DomaineAcademique />} />
        <Route path="/monguide-orientation-academique" element={<FormulaireOrientation />} />
        <Route path="/academique-resultats" element={<ResultatOrientation />} />
        {/*----- FIN ACADEMIQUE ------*/}

        {/*----- PROFESSIONNELLE ------*/}
        <Route path="/monguide-professionnel" element={<ProfessionnelleOrientation />} />
        <Route path="/monguide-domaine-professionnel" element={<DomaineProfessionnelle />} />
        <Route path="/monguide-orientation-professionnel" element={<FormulaireOrientationPro />} />
        <Route path="/professionnel-resultats" element={<ResultatOrientationPro />} />
        {/*----- FIN PROFESSIONNELLE ------*/}


        <Route path="/questionnaire" element={<QuestionnairePage />} />
        <Route path="/results" element={<ResultsPage />} />
      </Routes>
    </Router>
  );
}

export default App;