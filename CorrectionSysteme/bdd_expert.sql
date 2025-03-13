DROP DATABASE IF EXISTS expert_system;
CREATE DATABASE expert_system;
USE expert_system;

CREATE TABLE utilisateurs (
    id INT AUTO_INCREMENT PRIMARY KEY,
    nom VARCHAR(100) NOT NULL,
    niveau_etude VARCHAR(50) NOT NULL,
    filiere VARCHAR(50) NOT NULL,
    domaine_interet VARCHAR(100), 
    type ENUM('professionnelle', 'academique') NOT NULL,
    resultat VARCHAR(300), 
    date_creation TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE competences_utilisateur (
    id INT AUTO_INCREMENT PRIMARY KEY,
    utilisateur_id INT,
    competence VARCHAR(100) NOT NULL,
    FOREIGN KEY (utilisateur_id) REFERENCES utilisateurs(id) ON DELETE CASCADE
);

CREATE TABLE personnalite_utilisateur (
    id INT AUTO_INCREMENT PRIMARY KEY,
    utilisateur_id INT,
    trait VARCHAR(100) NOT NULL,
    FOREIGN KEY (utilisateur_id) REFERENCES utilisateurs(id) ON DELETE CASCADE
);



