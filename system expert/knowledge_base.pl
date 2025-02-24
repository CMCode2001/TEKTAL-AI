% ======= DOMAINES ET MÉTIERS =======
% Définition des domaines professionnels
domaine(informatique).
domaine(sante).
domaine(commerce).
domaine(ingenierie).
domaine(sciences_humaines).

% Définition des métiers et leurs domaines
metier(developpeur_web, informatique).
metier(data_scientist, informatique).
metier(cybersecurite, informatique).
metier(medecin, sante).
metier(infirmier, sante).
metier(commercial, commerce).
metier(chef_projet, commerce).
metier(ingenieur_civil, ingenierie).
metier(ingenieur_mecanique, ingenierie).
metier(psychologue, sciences_humaines).

% Niveau d'études requis (1=bac, 2=bac+2, 3=licence, 4=master, 5=doctorat)
niveau_etude_requis(developpeur_web, 3).
niveau_etude_requis(data_scientist, 4).
niveau_etude_requis(cybersecurite, 4).
niveau_etude_requis(medecin, 5).
niveau_etude_requis(infirmier, 3).
niveau_etude_requis(commercial, 2).
niveau_etude_requis(chef_projet, 4).
niveau_etude_requis(ingenieur_civil, 4).
niveau_etude_requis(ingenieur_mecanique, 4).
niveau_etude_requis(psychologue, 5).

% Description des métiers
description(developpeur_web, 'Conçoit et développe des sites et applications web').
description(data_scientist, 'Analyse des données complexes pour en extraire des connaissances').
description(cybersecurite, 'Protège les systèmes informatiques contre les menaces').
description(medecin, 'Diagnostique et traite les maladies').
description(infirmier, 'Prodigue des soins aux patients').
description(commercial, 'Développe les ventes et les relations clients').
description(chef_projet, 'Coordonne des équipes pour atteindre des objectifs').
description(ingenieur_civil, 'Conçoit et supervise des projets d\'infrastructure').
description(ingenieur_mecanique, 'Conçoit des systèmes mécaniques et machines').
description(psychologue, 'Étudie le comportement humain et aide les personnes en difficulté').

% Salaire moyen (en milliers d'euros par an)
salaire_moyen(developpeur_web, 40).
salaire_moyen(data_scientist, 55).
salaire_moyen(cybersecurite, 60).
salaire_moyen(medecin, 80).
salaire_moyen(infirmier, 35).
salaire_moyen(commercial, 40).
salaire_moyen(chef_projet, 55).
salaire_moyen(ingenieur_civil, 50).
salaire_moyen(ingenieur_mecanique, 48).
salaire_moyen(psychologue, 38).

% ======= COMPÉTENCES =======
% Types de compétences
competence(programmation).
competence(analyse_donnees).
competence(securite_informatique).
competence(sciences).
competence(communication).
competence(negociation).
competence(gestion_projet).
competence(mathematiques).
competence(physique).
competence(sciences_humaines).
competence(biologie).
competence(langues).

% Compétences requises pour chaque métier (compétence, métier, niveau requis 1-5)
requiert_competence(programmation, developpeur_web, 5).
requiert_competence(communication, developpeur_web, 3).
requiert_competence(mathematiques, developpeur_web, 3).

requiert_competence(programmation, data_scientist, 4).
requiert_competence(analyse_donnees, data_scientist, 5).
requiert_competence(mathematiques, data_scientist, 5).

requiert_competence(programmation, cybersecurite, 4).
requiert_competence(securite_informatique, cybersecurite, 5).
requiert_competence(analyse_donnees, cybersecurite, 3).

requiert_competence(sciences, medecin, 5).
requiert_competence(biologie, medecin, 5).
requiert_competence(communication, medecin, 4).

requiert_competence(sciences, infirmier, 3).
requiert_competence(biologie, infirmier, 4).
requiert_competence(communication, infirmier, 5).

requiert_competence(communication, commercial, 5).
requiert_competence(negociation, commercial, 5).
requiert_competence(langues, commercial, 3).

requiert_competence(gestion_projet, chef_projet, 5).
requiert_competence(communication, chef_projet, 5).
requiert_competence(negociation, chef_projet, 4).

requiert_competence(mathematiques, ingenieur_civil, 4).
requiert_competence(physique, ingenieur_civil, 4).
requiert_competence(gestion_projet, ingenieur_civil, 4).

requiert_competence(mathematiques, ingenieur_mecanique, 4).
requiert_competence(physique, ingenieur_mecanique, 5).
requiert_competence(programmation, ingenieur_mecanique, 3).

requiert_competence(sciences_humaines, psychologue, 5).
requiert_competence(communication, psychologue, 5).
requiert_competence(analyse_donnees, psychologue, 3).

% ======= MATIÈRES ACADÉMIQUES =======
% Importance des matières pour les compétences (matière, compétence, niveau d'importance 1-5)
importance_matiere(informatique, programmation, 5).
importance_matiere(informatique, securite_informatique, 5).
importance_matiere(statistiques, analyse_donnees, 5).
importance_matiere(mathematiques, analyse_donnees, 4).
importance_matiere(mathematiques, ingenierie, 5).
importance_matiere(physique, ingenierie, 5).
importance_matiere(biologie, sciences, 5).
importance_matiere(chimie, sciences, 4).
importance_matiere(francais, communication, 4).
importance_matiere(langues_etrangeres, communication, 3).
importance_matiere(langues_etrangeres, langues, 5).
importance_matiere(economie, negociation, 3).
importance_matiere(management, gestion_projet, 5).
importance_matiere(psychologie, sciences_humaines, 5).
importance_matiere(sociologie, sciences_humaines, 4).

% ======= ÉCOLES ET FORMATIONS =======
% Définition des écoles
ecole(epita, informatique, paris).
ecole(42, informatique, paris).
ecole(isep, informatique, paris).
ecole(faculte_medecine_paris, sante, paris).
ecole(ifsi_lyon, sante, lyon).
ecole(hec, commerce, paris).
ecole(essec, commerce, cergy).
ecole(centrale_paris, ingenierie, paris).
ecole(insa_lyon, ingenierie, lyon).
ecole(paris_descartes, sciences_humaines, paris).

% Formations proposées (école, niveau, spécialisation)
formation(epita, 5, ingenieur_informatique).
formation(42, 3, developpement_logiciel).
formation(isep, 5, ingenieur_reseaux).
formation(faculte_medecine_paris, 5, medecine_generale).
formation(ifsi_lyon, 3, soins_infirmiers).
formation(hec, 5, management).
formation(essec, 5, commerce_international).
formation(centrale_paris, 5, ingenierie_generale).
formation(insa_lyon, 5, mecanique).
formation(paris_descartes, 5, psychologie_clinique).

% Formations pour métiers spécifiques
formation_pour_metier(developpeur_web, epita, ingenieur_informatique).
formation_pour_metier(developpeur_web, 42, developpement_logiciel).
formation_pour_metier(data_scientist, epita, ingenieur_informatique).
formation_pour_metier(cybersecurite, isep, ingenieur_reseaux).
formation_pour_metier(medecin, faculte_medecine_paris, medecine_generale).
formation_pour_metier(infirmier, ifsi_lyon, soins_infirmiers).
formation_pour_metier(commercial, essec, commerce_international).
formation_pour_metier(chef_projet, hec, management).
formation_pour_metier(ingenieur_civil, centrale_paris, ingenierie_generale).
formation_pour_metier(ingenieur_mecanique, insa_lyon, mecanique).
formation_pour_metier(psychologue, paris_descartes, psychologie_clinique).


% ======= PRÉDICATS SUPPLÉMENTAIRES POUR INFÉRENCE =======

% Traits de personnalité requis pour les métiers
trait_personnalite_requis(developpeur_web, creativite, 4).
trait_personnalite_requis(data_scientist, rigueur, 5).
trait_personnalite_requis(medecin, empathie, 5).
trait_personnalite_requis(infirmier, patience, 4).
trait_personnalite_requis(commercial, persuasion, 5).
trait_personnalite_requis(chef_projet, leadership, 5).
trait_personnalite_requis(ingenieur_civil, precision, 4).
trait_personnalite_requis(ingenieur_mecanique, logique, 5).
trait_personnalite_requis(psychologue, ecoute, 5).

% Traits de personnalité d'une personne
trait_personnalite(jean, creativite, 4).
trait_personnalite(jean, rigueur, 3).
trait_personnalite(marie, empathie, 5).
trait_personnalite(marie, patience, 4).

% Préférences de localisation et budget
preference_localisation(jean, paris).
preference_localisation(marie, lyon).
budget_formation(jean, 20000).
budget_formation(marie, 30000).

% Coût des formations
cout_formation(epita, ingenieur_informatique, 15000).
cout_formation(42, developpement_logiciel, 0).
cout_formation(faculte_medecine_paris, medecine_generale, 5000).
cout_formation(ifsi_lyon, soins_infirmiers, 2000).
cout_formation(hec, management, 30000).
cout_formation(essec, commerce_international, 25000).

% Prestige des écoles
prestige_ecole(epita, 4).
prestige_ecole(42, 5).
prestige_ecole(faculte_medecine_paris, 5).
prestige_ecole(hec, 5).
prestige_ecole(essec, 4).

% Compétences développées par les formations
competence_developpee_par_formation(ingenieur_informatique, programmation, 5).
competence_developpee_par_formation(developpement_logiciel, programmation, 4).
competence_developpee_par_formation(medecine_generale, biologie, 5).
competence_developpee_par_formation(soins_infirmiers, communication, 4).
competence_developpee_par_formation(management, gestion_projet, 5).
competence_developpee_par_formation(commerce_international, negociation, 4).

% Durée des formations
duree_formation(epita, ingenieur_informatique, 5).
duree_formation(42, developpement_logiciel, 3).
duree_formation(faculte_medecine_paris, medecine_generale, 10).
duree_formation(ifsi_lyon, soins_infirmiers, 3).
duree_formation(hec, management, 2).
duree_formation(essec, commerce_international, 2).


% ======= RÈGLES D'INFÉRENCE =======
% Compatibilité métier/profil
compatible_avec_metier(Personne, Metier, Score) :-
    niveau_etude(Personne, NiveauEtude),
    niveau_etude_requis(Metier, NiveauRequisEtude),
    NiveauEtude >= NiveauRequisEtude,
    calcul_score_competences(Personne, Metier, ScoreCompetences),
    calcul_score_interet(Personne, Metier, ScoreInteret),
    Score is (ScoreCompetences * 0.7 + ScoreInteret * 0.3).

% Calculer le score basé sur les compétences
calcul_score_competences(Personne, Metier, Score) :-
    findall(S, (
        requiert_competence(Comp, Metier, NiveauRequis),
        niveau_competence(Personne, Comp, NiveauPersonne),
        S is min(NiveauPersonne / NiveauRequis, 1) * 100
    ), Scores),
    sum_list(Scores, Total),
    length(Scores, N),
    Score is Total / N.

% Calculer le score basé sur les intérêts
calcul_score_interet(Personne, Metier, Score) :-
    metier(Metier, Domaine),
    interet(Personne, Domaine, NiveauInteret),
    Score is NiveauInteret * 20.  % Convertir une échelle 1-5 en pourcentage

% Matières à renforcer
matieres_a_renforcer(Personne, Metier, Matieres) :-
    findall(matiere(Matiere, NiveauImportance, NiveauActuel), (
        requiert_competence(Comp, Metier, _),
        importance_matiere(Matiere, Comp, NiveauImportance),
        niveau_matiere(Personne, Matiere, NiveauActuel),
        NiveauActuel < NiveauImportance
    ), Matieres).

% Recommandation d'écoles
recommander_ecoles(Metier, Ecoles) :-
    findall(Ecole, formation_pour_metier(Metier, Ecole, _), Ecoles).

% Plan de développement
plan_developpement(Personne, Metier, Plan) :-
    matieres_a_renforcer(Personne, Metier, MatieresARenforcer),
    niveau_etude(Personne, NiveauEtudeActuel),
    niveau_etude_requis(Metier, NiveauEtudeRequis),
    recommander_ecoles(Metier, Ecoles),
    Plan = plan(
        metier(Metier),
        niveau_actuel(NiveauEtudeActuel),
        niveau_requis(NiveauEtudeRequis),
        matieres_a_renforcer(MatieresARenforcer),
        ecoles_recommandees(Ecoles)
    ).

% ======= EXEMPLES DE PROFILS (à utiliser pour les tests) =======
% Profil utilisateur (exemple)
niveau_etude(jean, 3).  % Jean a un niveau licence
niveau_competence(jean, programmation, 4).
niveau_competence(jean, communication, 3).
niveau_competence(jean, mathematiques, 4).
niveau_matiere(jean, informatique, 4).
niveau_matiere(jean, mathematiques, 4).
niveau_matiere(jean, langues_etrangeres, 3).
interet(jean, informatique, 5).
interet(jean, ingenierie, 3).
interet(jean, commerce, 2).

niveau_etude(marie, 4).  % Marie a un niveau master
niveau_competence(marie, sciences, 5).
niveau_competence(marie, biologie, 5).
niveau_competence(marie, communication, 3).
niveau_matiere(marie, biologie, 5).
niveau_matiere(marie, chimie, 4).
niveau_matiere(marie, langues_etrangeres, 3).
interet(marie, sante, 5).
interet(marie, sciences_humaines, 4).
interet(marie, informatique, 2).