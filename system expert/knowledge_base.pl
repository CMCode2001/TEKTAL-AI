% ======= DOMAINES ET MÉTIERS =======
% Définition des domaines professionnels
:- module(base_de_connaissance, [domaine/1, metier/2]).

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
trait_personnalite_requis(developpeur_web, creativite, 4).
trait_personnalite_requis(data_scientist, rigueur, 5).
trait_personnalite_requis(medecin, empathie, 5).
trait_personnalite_requis(infirmier, patience, 4).
trait_personnalite_requis(commercial, persuasion, 5).
trait_personnalite_requis(chef_projet, leadership, 5).
trait_personnalite_requis(ingenieur_civil, precision, 4).
trait_personnalite_requis(ingenieur_mecanique, logique, 5).
trait_personnalite_requis(psychologue, ecoute, 5).

trait_personnalite(jean, creativite, 4).
trait_personnalite(jean, rigueur, 3).
trait_personnalite(marie, empathie, 5).
trait_personnalite(marie, patience, 4).

preference_localisation(jean, paris).
preference_localisation(marie, lyon).
budget_formation(jean, 20000).
budget_formation(marie, 30000).

cout_formation(epita, ingenieur_informatique, 15000).
cout_formation(42, developpement_logiciel, 0).
cout_formation(faculte_medecine_paris, medecine_generale, 5000).
cout_formation(ifsi_lyon, soins_infirmiers, 2000).
cout_formation(hec, management, 30000).
cout_formation(essec, commerce_international, 25000).

prestige_ecole(epita, 4).
prestige_ecole(42, 5).
prestige_ecole(faculte_medecine_paris, 5).
prestige_ecole(hec, 5).
prestige_ecole(essec, 4).

competence_developpee_par_formation(ingenieur_informatique, programmation, 5).
competence_developpee_par_formation(developpement_logiciel, programmation, 4).
competence_developpee_par_formation(medecine_generale, biologie, 5).
competence_developpee_par_formation(soins_infirmiers, communication, 4).
competence_developpee_par_formation(management, gestion_projet, 5).
competence_developpee_par_formation(commerce_international, negociation, 4).

duree_formation(epita, ingenieur_informatique, 5).
duree_formation(42, developpement_logiciel, 3).
duree_formation(faculte_medecine_paris, medecine_generale, 10).
duree_formation(ifsi_lyon, soins_infirmiers, 3).
duree_formation(hec, management, 2).
duree_formation(essec, commerce_international, 2).