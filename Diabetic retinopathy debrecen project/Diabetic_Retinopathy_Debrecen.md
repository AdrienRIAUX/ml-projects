Projet d’apprentissage statistique
================
Adrien Riaux
18/01/2022

# Motivation et positionnement du projet

J’ai choisi le sujet concernant la rétinopathie diabétique. Maladie
résultant de la complication d’un diabète et de l’atteinte des vaisseaux
de la rétine, mettant en jeu un pronostic visuel. C’est une maladie
compliquée à traiter et qui a de graves conséquences. C’est pourquoi
l’analyse de ce dataset, ainsi que la création d’un modèle prédictif
sont très intéressants pour moi.

De plus le *diabetic retinopathy debrecen dataset* nous fournit un
nombre suffisant d’individus et de varible pour mener à bien cette
analyse (ce qui devrait nous éviter d’être en underfitting ou
overfitting). Il semblerait qu’il n’y ait pas de valeurs manquantes dans
le dataset (même si nous allons le vérifier par la suite).

La tâche à réaliser pour cette analyse est une classification, puisqu’il
faut séparer les cas des patients sains de ceux malades.

L’ensemble des données contient des caractéristiques extraites des
images *Messidor* dans le but de prédire si une personne est atteinte de
rétinopathie diabétique. Toutes les caractéristiques représentent soit
une lésion détectée, soit une caratéristique descriptive d’une partie
anatomique, soit un descripteur au niveau de l’image.

# Analyse descriptive

Les informations qui nous sont fournis sur le dataset sont les suivantes
:

V1) Le résultat binaire de l’évaluation de la qualité. 0 = mauvaise
qualité 1 = qualité suffisante.

V2) Le résultat binaire du pré-dépistage, où 1 indique une anomalie
rétinienne sévère et 0 son absence.

V3-V8) Les résultats de la détection MA. Chaque valeur de
caractéristique représente le nombre d’AM trouvées aux niveaux de
confiance alpha = 0,5, . . . , 1, respectivement.

V9-V16) Contiennent les mêmes informations que les variables 3-8, mais
pour les exsudats. Comme les exsudats sont représentés par un ensemble
de points plutôt que le nombre de pixels construisant les lésions, ces
caractéristiques sont normalisées en divisant le nombre de lésions avec
le diamètre de la ROI pour compenser des images de différentes tailles.

V17) La distance euclidienne du centre de la macula et le centre du
disque optique pour fournir des informations importantes concernant
l’état du patient. Cette fonctionnalité est également normalisé avec le
diamètre de la ROI.

V18) Le diamètre du disque optique.

V19) Le résultat binaire de la classification AM/FM.

V20) Étiquette de classe. 1 = contient des signes de DR (label cumulatif
pour les classes Messidor 1, 2, 3), 0 = aucun signe de DR.

Dans un premier temps, nous allons procéder à une analyse descriptive
des données. En particulier, la distribution des différentes variables,
et des liens ou des dépendances entres celles-ci.

Pour cela il nous faut commencer par charger nos données dans un
dataframe. Ce qui facilite énormément les manipulation.

``` r
#On récupère nos données depuis le fichier texte
data = read.table("messidor_features.txt", header = FALSE, sep = ",")
head(data)
```

    ##   V1 V2 V3 V4 V5 V6 V7 V8       V9       V10       V11      V12      V13
    ## 1  1  1 22 22 22 19 18 14 49.89576 17.775994  5.270920 0.771761 0.018632
    ## 2  1  1 24 24 22 18 16 13 57.70994 23.799994  3.325423 0.234185 0.003903
    ## 3  1  1 62 60 59 54 47 33 55.83144 27.993933 12.687485 4.852282 1.393889
    ## 4  1  1 55 53 53 50 43 31 40.46723 18.445954  9.118901 3.079428 0.840261
    ## 5  1  1 44 44 44 41 39 27 18.02625  8.570709  0.410381 0.000000 0.000000
    ## 6  1  1 44 43 41 41 37 29 28.35640  6.935636  2.305771 0.323724 0.000000
    ##        V14      V15      V16      V17      V18 V19 V20
    ## 1 0.006864 0.003923 0.003923 0.486903 0.100025   1   0
    ## 2 0.003903 0.003903 0.003903 0.520908 0.144414   0   0
    ## 3 0.373252 0.041817 0.007744 0.530904 0.128548   0   1
    ## 4 0.272434 0.007653 0.001531 0.483284 0.114790   0   0
    ## 5 0.000000 0.000000 0.000000 0.475935 0.123572   0   1
    ## 6 0.000000 0.000000 0.000000 0.502831 0.126741   0   1

Ce tableau nous permet d’observer notre dataset de manière globale dans
un premier temps. Il nous donne un aperçu des valeurs prisent par chaque
variables, ainsi que leur type (int, num…). Les variables 1, 2, 19 et 20
sont des variables de type catégorielles prenant des valeurs entre
{0,1}. Pour en être sûr nous allons vérifier les valeurs uniques de ces
variables.

``` r
vec <- c(1,2,19,20) #Numéro des colonnes qui semble être catégorielles
for (i in vec) {
  print(unique(data[,i]))
}
```

    ## [1] 1 0
    ## [1] 1 0
    ## [1] 1 0
    ## [1] 0 1

Ce résultat nous confirme que nous avons bien 4 variables de type
catégorielles. De plus, à l’aide des informations qui nous sont fournis
sur le site du dataset, nous savons que la dernière variable est celle
du résultat de l’analyse. Elle nous indique si un patient est malade ou
sain.

Désormais nous allons convertir le type de ces 4 variables en un type
catégorielle (à savoir en factor pour le langage R).

``` r
#Transformation de type int en factor
for (i in vec){
  data[,i] <- as.factor(data[,i])
}

sapply(data, class)
```

    ##        V1        V2        V3        V4        V5        V6        V7        V8 
    ##  "factor"  "factor" "integer" "integer" "integer" "integer" "integer" "integer" 
    ##        V9       V10       V11       V12       V13       V14       V15       V16 
    ## "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" 
    ##       V17       V18       V19       V20 
    ## "numeric" "numeric"  "factor"  "factor"

Maintenant que nous avons le bon type pour chacune de nos variables,
nous pouvons vérifer si nous avons des valeurs nulles. La commande str()
nous donne de nouveau des informations sur les valeurs et le type de
chaque variables. Sachant que dans la description du dataset, il y a
indiqué 1151 observations et aucune valeur manquante.

``` r
#On vérifie le type de chaque variables et si il y des valeurs nulles
str(data)
```

    ## 'data.frame':    1151 obs. of  20 variables:
    ##  $ V1 : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ V2 : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 1 2 2 2 ...
    ##  $ V3 : int  22 24 62 55 44 44 29 6 22 79 ...
    ##  $ V4 : int  22 24 60 53 44 43 29 6 21 75 ...
    ##  $ V5 : int  22 22 59 53 44 41 29 6 18 73 ...
    ##  $ V6 : int  19 18 54 50 41 41 27 6 15 71 ...
    ##  $ V7 : int  18 16 47 43 39 37 25 2 13 64 ...
    ##  $ V8 : int  14 13 33 31 27 29 16 1 10 47 ...
    ##  $ V9 : num  49.9 57.7 55.8 40.5 18 ...
    ##  $ V10: num  17.78 23.8 27.99 18.45 8.57 ...
    ##  $ V11: num  5.27 3.33 12.69 9.12 0.41 ...
    ##  $ V12: num  0.772 0.234 4.852 3.079 0 ...
    ##  $ V13: num  0.0186 0.0039 1.3939 0.8403 0 ...
    ##  $ V14: num  0.00686 0.0039 0.37325 0.27243 0 ...
    ##  $ V15: num  0.00392 0.0039 0.04182 0.00765 0 ...
    ##  $ V16: num  0.00392 0.0039 0.00774 0.00153 0 ...
    ##  $ V17: num  0.487 0.521 0.531 0.483 0.476 ...
    ##  $ V18: num  0.1 0.144 0.129 0.115 0.124 ...
    ##  $ V19: Factor w/ 2 levels "0","1": 2 1 1 1 1 1 1 2 1 1 ...
    ##  $ V20: Factor w/ 2 levels "0","1": 1 1 2 1 2 2 2 1 2 2 ...

Il n’y a pas de valeurs nulle dans notre dataset. De plus chaque
variable possède désormais le bon type. Nous allons pouvoir procéder à
l’analyse préliminaire de nos données.

``` r
#On applique sur chaque colonne des statistiques de bases (min, max, moyenne, quartile, etc.)
summary(data)
```

    ##  V1       V2             V3               V4               V5        
    ##  0:   4   0:  94   Min.   :  1.00   Min.   :  1.00   Min.   :  1.00  
    ##  1:1147   1:1057   1st Qu.: 16.00   1st Qu.: 16.00   1st Qu.: 15.00  
    ##                    Median : 35.00   Median : 35.00   Median : 32.00  
    ##                    Mean   : 38.43   Mean   : 36.91   Mean   : 35.14  
    ##                    3rd Qu.: 55.00   3rd Qu.: 53.00   3rd Qu.: 51.00  
    ##                    Max.   :151.00   Max.   :132.00   Max.   :120.00  
    ##        V6              V7              V8              V9          
    ##  Min.   :  1.0   Min.   : 1.00   Min.   : 1.00   Min.   :  0.3493  
    ##  1st Qu.: 14.0   1st Qu.:11.00   1st Qu.: 8.00   1st Qu.: 22.2716  
    ##  Median : 29.0   Median :25.00   Median :18.00   Median : 44.2491  
    ##  Mean   : 32.3   Mean   :28.75   Mean   :21.15   Mean   : 64.0967  
    ##  3rd Qu.: 48.0   3rd Qu.:43.00   3rd Qu.:32.00   3rd Qu.: 87.8041  
    ##  Max.   :105.0   Max.   :97.00   Max.   :89.00   Max.   :403.9391  
    ##       V10               V11               V12                V13          
    ##  Min.   :  0.000   Min.   :  0.000   Min.   : 0.00000   Min.   : 0.00000  
    ##  1st Qu.:  7.939   1st Qu.:  1.249   1st Qu.: 0.08155   1st Qu.: 0.00000  
    ##  Median : 17.038   Median :  4.423   Median : 0.48483   Median : 0.02225  
    ##  Mean   : 23.088   Mean   :  8.705   Mean   : 1.83649   Mean   : 0.56074  
    ##  3rd Qu.: 31.306   3rd Qu.: 11.767   3rd Qu.: 1.92165   3rd Qu.: 0.19195  
    ##  Max.   :167.131   Max.   :106.070   Max.   :59.76612   Max.   :51.42321  
    ##       V14                 V15                V16                V17        
    ##  Min.   : 0.000000   Min.   :0.000000   Min.   :0.000000   Min.   :0.3678  
    ##  1st Qu.: 0.000000   1st Qu.:0.000000   1st Qu.:0.000000   1st Qu.:0.5029  
    ##  Median : 0.001554   Median :0.000000   Median :0.000000   Median :0.5233  
    ##  Mean   : 0.212290   Mean   :0.085674   Mean   :0.037225   Mean   :0.5232  
    ##  3rd Qu.: 0.038450   3rd Qu.:0.004832   3rd Qu.:0.003851   3rd Qu.:0.5437  
    ##  Max.   :20.098605   Max.   :5.937799   Max.   :3.086753   Max.   :0.5922  
    ##       V18          V19     V20    
    ##  Min.   :0.05791   0:764   0:540  
    ##  1st Qu.:0.09580   1:387   1:611  
    ##  Median :0.10662                  
    ##  Mean   :0.10843                  
    ##  3rd Qu.:0.11959                  
    ##  Max.   :0.21920

La commande summary nous donne des informations statistiques de base par
variables (sauf pour les variables catégorielles, ce qui n’aurait pas de
sens).

On remarque dans un premier temps que pour nos variables de type “int”
{V3 à V8}, le minimum est toujours de 1. La médiane et la moyenne sont
sensiblement identiques aussi, ce qui nous confirme que ces variables
concernent toutes des informations proches. Plus particulièrement les
résultats de la détection AM. Il peut être intéressant de les analyser à
part des autres dans notre dataset.

On ne retrouve pas ces similarités dans les variables de type “num”.

Pour les variables 1 et 2, on peut remarquer que le nombre de 0 est très
faible. Ce qui met en valeur le faible nombre d’évaluation de mauvaise
qualité. Cela nous indique aussi que la majorité des patients ont eu un
résultat positif au pré-dépistage. Alors que pour la variable 20, on
peut voir que l’on à peu près le même nombre de patients malades que
sains. On peut donc en conclure sur la faible efficacité du
pré-dépistage.

On peut observer le nombre en % de cas malades/sains, afin d’avoir une
meilleur visibilité sur la variable 20.

``` r
#On visualise le nombre de cas positif et négatif de notre dataset
my_count <- table(data$V20)
pct <- round(my_count/sum(my_count)*100)
lbls <- c("Negative", "Positive")
lbls <- paste(lbls,pct)
lbls <- paste(lbls,"%", sep = "")

pie(my_count, labels = lbls, main = "Répartition des individus malades/sains", col = c("#F26D6D","#41D9D9"))
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
Ici en rouge, les patients negatifs au RD, et en bleu les patients
positifs au RD. Ce qui veut dire que les classes sont équilibrées. Cela
nous facilitera la tâche lors de la classification supervisé, car nous
n’auront pas à jouer sur les poids des classes pour rééquilibrer des
classes disproportionnées.

Sachant que la variable 20 constitue notre “target”, nous allons
analyser la répartition des autres variables en fonction de celle-ci.
Pour cela nous utilisons des boxplots. En séparant les cas sains des cas
malades.(Il suffit de cliquer sur un des graphiques pour l’afficher en
version grande plus bas).

``` r
library(ggplot2)
```

    ## Warning: le package 'ggplot2' a été compilé avec la version R 4.1.2

``` r
#A l'aide de boxplot on regarde la distribution des individus pour chaque variables
for (i in 3:17){
  g <- ggplot(data, aes(x = V20, y = data[,i]))
  print(g + geom_boxplot(aes(color = V20))
  + xlab("Patient sain/malade")
  + ylab(paste("V",i)))
  
}
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-6.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-7.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-8.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-9.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-10.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-11.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-12.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-13.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-14.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-7-15.png)<!-- -->
Ces boxplots sont très intéressants à analyser. Car ils nous donnent des
informations sur la répartitions des valeurs par variable, et en
fonction de la variable “target”. On peut voir pour les variables 3 à 8
une légère différence de la répartition des valeurs entre un patient
sain et un malade. Il semblerait que les patients malades aient des
résultats plus élevés à la détection MA.

On remarque aussi que pour certaines variables (12 à 16), les patients
malades semblent plus souvent répertoriés comme des “outliers” (valeurs
abérantes).

Nous allons donc maintenant nous intéresser d’un peu plus près au
variable 3 à 8.

``` r
#Analyse des variables 3 à 8 
library(GGally)
```

    ## Warning: le package 'GGally' a été compilé avec la version R 4.1.2

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
GGally::ggpairs(data[c(3:8,20)])
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
Ce graphique met en valeur deux points très importants pour les
variables 3 à 8 :

Tout d’abord, elles ont toutes la même distribution. Ce qui semble
logique puisqu’elles sont toutes liées à un test commun.

Ensuite, ces variables ont de très forte corrélation (positive). Ce qui
veut dire qu’il y a de fortes dépendances entre ces variables.

On peut donc s’intéresser aux corrélations entre toutes les variables de
notre dataset. Pour calculer une matrice de corrélation il nous faut
uniquement des valeurs numérique. On peut donc enlever les variables
catégorielles de notre dataset. Sachant qu’il serait très peu pertinent
de les inclure dans l’analyse, puisqu’elles constituent les résultats
d’analyses postérieurs à celles des variables 3 à 18.

On commence par définir un dataframe ne contenant que les informations
que l’on veut traiter.

``` r
#Pour avoir la matrice de corrélation entre nos variables, il faut qu'elles soient de type numérique
corr_data <- cor(data[3:18])
ncol(corr_data)
```

    ## [1] 16

On a bien que les 16 colonnes de notre dataset, qui sont de type
“numeric”.

On peut donc construire notre matrice de corrélation. Pour cela on
construit d’abord un tableau contenant la corrélation entre chaque
combinaisons de variables possibles.

``` r
#Préparation des données pour avoir la corrélation entre variables
library(reshape2)
```

    ## Warning: le package 'reshape2' a été compilé avec la version R 4.1.2

``` r
library(scales)
```

    ## Warning: le package 'scales' a été compilé avec la version R 4.1.2

``` r
melt_data <- reshape2::melt(corr_data, varnames = c("x","y"), value.name = "Correlation" )
melt_data <- melt_data[order(melt_data$Correlation),]
```

Ensuite on peut afficher à l’aide d’une heatmap, notre matrice de
corrélation.

``` r
#On utilise une heatmap pour mieux visualiser les corrélations
ggplot(melt_data, aes(x = x, y = y)) +
  geom_tile(aes(fill = Correlation)) +
  scale_fill_gradient2(low = muted("yellow"), mid = "white", high = muted("red"), guide = guide_colorbar(ticks = FALSE, barheight = 10), limits = c(-1,1)) +
  theme_minimal() + 
  labs(x = NULL, y = NULL)
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
On retrouve nos fortes corrélations observées auparavant sur les
variables 3 à 8. On peut aussi observer des fortes corrélations entre
les variables 9 à 16.

Cependant on peut remarquer qu’il n’y a pas ou une très faible
corrélation entre les variables qui constituent les résultats de la
détection MA (V3-V8) et ceux des exsudats (V9-V16). Il est donc
important de prendre ces variables en compte lors de notre analyse.

On peut visualiser la matrice de corrélation d’une autre manière, en y
ajoutant un dendrogramme. Ce qui peut être intéressant pour voir les
couples de variables.

``` r
#Corrélation des variables avec ajout d'un dendrogramme sur la heatmap
heatmap(corr_data, scale = "column", margins = c(2,2))
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
On observe 3 groupes, les deux précédemments observés et un autre qui
est formé des variables 17 et 18. Ce dendrogramme nous montre aussi
qu’il pourrait être intéressant de regarder le nuage de points entre
deux variables qui se suivent (par exemple V13 et V14), car il
semblerait que dans notre cas, les premiers duo sont réalisé de cette
manière par le dendrogramme.

On peut aussi s’intéresser à la distribution des variables.

``` r
#Distribution des variables
for (i in 3:18){
  col <- data[,i]
  dist <- density(col)
  
  hist(col, xlab="",ylab="", main = paste("Histogram of variable", i))
  
  par(new = T) #Définit que l'on travail sur le même graphique
  plot(dist, col = "red", axes=F, xlab="", ylab="", main="")
}
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-5.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-6.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-7.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-8.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-9.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-10.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-11.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-12.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-13.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-14.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-15.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-13-16.png)<!-- -->
On peut remarquer que le groupe V3-V8 suit une même distribution, qu’il
en est de même pour le groupe V9-V16 et le groupe V17-V18.

On peut en conclure que nous ne sommes pas en présence de distribution
normale.

On s’intéresse donc maintenant à une brève analyse des nuages de points
entre les variables.

``` r
#Analyse des nuages de points
for (i in 3:17){
    g <- ggplot(data, aes(x = data[,i], y = data[,i+1]))
    print(g + geom_point(aes(color = V20)) + facet_wrap(~V20)
    + xlab(paste("V",i,sep = ""))
    + ylab(paste("V",i+1,sep = "")))
}
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-5.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-6.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-7.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-8.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-9.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-10.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-11.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-12.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-13.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-14.png)<!-- -->![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-14-15.png)<!-- -->
Ces graphiques nous aident à voir que en observant certains nuages de
points, il est aisé de différencier les points concernants une personne
malade d’une personne saine. Ce qui nous conforte dans l’idée qu’une
classification supervisée peut mener à de bon résultat.

Nous allons maintenant passer à l’étape suivante du projet.

# Classification non supervisée

Dans cette partie, nous allons supprimer la variable classe de notre
dataset. Dans le but d’utiliser trois méthodes vu en cours : KMeans, PAM
et CAH.

Pour chaque méthodes, nous déterminerons le bon nombre de profil type,
en extraire les profils et les interpréter.

``` r
# On commence par extraire la variable target de notre dataset
unsu_data <- data[-20]
head(unsu_data)
```

    ##   V1 V2 V3 V4 V5 V6 V7 V8       V9       V10       V11      V12      V13
    ## 1  1  1 22 22 22 19 18 14 49.89576 17.775994  5.270920 0.771761 0.018632
    ## 2  1  1 24 24 22 18 16 13 57.70994 23.799994  3.325423 0.234185 0.003903
    ## 3  1  1 62 60 59 54 47 33 55.83144 27.993933 12.687485 4.852282 1.393889
    ## 4  1  1 55 53 53 50 43 31 40.46723 18.445954  9.118901 3.079428 0.840261
    ## 5  1  1 44 44 44 41 39 27 18.02625  8.570709  0.410381 0.000000 0.000000
    ## 6  1  1 44 43 41 41 37 29 28.35640  6.935636  2.305771 0.323724 0.000000
    ##        V14      V15      V16      V17      V18 V19
    ## 1 0.006864 0.003923 0.003923 0.486903 0.100025   1
    ## 2 0.003903 0.003903 0.003903 0.520908 0.144414   0
    ## 3 0.373252 0.041817 0.007744 0.530904 0.128548   0
    ## 4 0.272434 0.007653 0.001531 0.483284 0.114790   0
    ## 5 0.000000 0.000000 0.000000 0.475935 0.123572   0
    ## 6 0.000000 0.000000 0.000000 0.502831 0.126741   0

Maintenant que nous avons notre “unsupervised dataset”, nous pouvons
appliquer les différents algorithmes cités auparavant.

Nous allons commencer par le KMeans. Mais avant d’utiliser cet
algorithme, il nous faut déterminer le nombre de clusters adéquat. Cela
dans le but de maximiser nos résultats. On utilise donc la méthode du
coude.

Car il y a une composante aléatoire dans le clustering, nous définissons
le “seed” pour générer un résultat reproductible.

``` r
#Methode du coude
#Initialise nos ratio à zéro
ratio_inter <- 0
ratio_intra <- 0
set.seed(30)

#Dans une boucle on test le KMeans pour k clusters
for (k in 1:10){
  resultat <- kmeans(unsu_data, k)
  ratio_inter[k] <- resultat$betweenss/resultat$totss
  ratio_intra[k] <- resultat$tot.withinss/resultat$totss
}

#Affichage
plot(ratio_inter, type="l", col = "red", )
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
plot(ratio_intra, type="l", col = "blue")
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->
Avec les deux graphiques, nous pouvons voir que la région du coude se
situe autour de 4. Nous pouvons donc en conclure que 4 clusters est
l’idéal pour réaliser notre apprentissage non supervisé.

Il existe une autre méthode afin de déterminer le nombre de clusters
pour notre algorithme KMeans. Il s’agit de la fonction “FitKMeans”,
disponible dans la librairie “useful”.

Elle compare essentiellement le rapport de la somme des carrés
intra-cluster pour un cluster avec k clusters, en tenant compte du
nombre de lignes et de clusters. Si ce nombre est supérieur à 10, cela
vaut la peine d’utiliser k+1 clusters.

``` r
library(useful)
```

    ## Warning: le package 'useful' a été compilé avec la version R 4.1.2

``` r
best_km <- FitKMeans(unsu_data, max.clusters = 10, seed = 30)

best_km
```

    ##   Clusters  Hartigan AddCluster
    ## 1        2 841.02974       TRUE
    ## 2        3 781.06630       TRUE
    ## 3        4 256.47383       TRUE
    ## 4        5 239.72574       TRUE
    ## 5        6 188.80042       TRUE
    ## 6        7 146.04569       TRUE
    ## 7        8 100.62268       TRUE
    ## 8        9 123.97341       TRUE
    ## 9       10  69.89303       TRUE

On va observer sur un graphique afin d’avoir une meilleur compréhension
du résultat.

``` r
PlotHartigan(best_km)
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
Ce qui nous indique ces résultats, c’est qu’il faut faudrait ajouter des
clusters afin de répondre à la métrics d’Hartigan et afin d’avoir un
clustering plus précis. Cependant le nombre de clusters devenant trop
éléver, alors qu’initialement il n’y a que deux classes dans le dataset,
nous allons rester sur nos précedents résultats. De plus, on observe ici
un grand changement lors du passage à 4 clusters. Ce qui nous conforte
dans l’utilisation de la méthode du coude pour notre démarche.

On utilise donc l’agorithme du KMeans avec 4 clusters.

``` r
#A l'aide de la methode du coude on peut dire que le meilleur k est 4 
set.seed(30)
km_data <- kmeans(unsu_data, centers = 4)
km_data
```

    ## K-means clustering with 4 clusters of sizes 398, 456, 206, 91
    ## 
    ## Cluster means:
    ##          V1        V2       V3       V4       V5       V6       V7       V8
    ## 1 0.9899497 0.8894472 19.70352 19.09045 18.25377 16.64322 14.55528 10.77889
    ## 2 1.0000000 0.9320175 62.67982 60.25439 57.99781 53.95395 49.11404 36.57895
    ## 3 1.0000000 0.9320388 24.70388 23.48058 21.60680 19.21359 15.97573 11.14078
    ## 4 1.0000000 0.9450549 29.86813 28.26374 25.09890 21.85714 17.67033 11.86813
    ##          V9      V10       V11       V12       V13        V14        V15
    ## 1  37.79294 13.77515  3.518460 0.5615569 0.2063773 0.07154077 0.02459113
    ## 2  34.17715 15.31933  5.043275 1.2806286 0.4845182 0.19495004 0.09135515
    ## 3 116.76679 39.97488 16.297219 3.1247486 0.8166432 0.28917827 0.11963939
    ## 4 209.83453 64.52036 32.546142 7.2816976 1.9132154 0.74070891 0.24747255
    ##          V16       V17       V18        V19
    ## 1 0.01052175 0.5256735 0.1089041 0.42211055
    ## 2 0.04507928 0.5226765 0.1092867 0.05701754
    ## 3 0.04746321 0.5217412 0.1069664 0.63592233
    ## 4 0.09148246 0.5184623 0.1053935 0.68131868
    ## 
    ## Clustering vector:
    ##    [1] 1 1 2 2 2 2 1 1 1 2 3 1 2 2 2 1 2 1 2 3 1 4 1 1 3 1 2 4 2 2 1 2 4 4 2 1 1
    ##   [38] 2 2 1 2 1 4 1 1 1 2 1 3 1 1 1 1 1 2 2 1 3 2 4 1 3 1 3 2 1 2 2 3 2 2 1 2 2
    ##   [75] 2 2 1 1 1 3 2 1 2 4 1 2 3 1 1 1 1 2 2 3 3 3 2 3 2 3 2 2 3 1 3 2 3 1 1 2 1
    ##  [112] 1 2 3 2 1 2 2 3 2 3 1 2 1 1 1 2 2 2 4 2 1 2 2 1 1 1 2 3 1 2 1 3 1 3 2 1 2
    ##  [149] 1 4 1 1 2 1 1 2 3 2 2 2 1 2 3 1 2 1 3 2 3 1 1 2 2 1 1 1 2 1 2 2 1 1 4 2 2
    ##  [186] 4 2 2 2 2 2 4 2 2 2 1 2 2 4 1 1 1 3 3 1 2 1 1 2 2 1 2 4 3 1 3 2 4 2 2 2 1
    ##  [223] 1 4 2 3 1 2 1 2 1 1 3 2 2 1 1 1 1 3 2 3 2 1 1 3 1 1 2 1 2 2 1 3 2 4 2 1 1
    ##  [260] 4 1 2 1 1 1 2 1 1 3 2 1 2 3 2 1 2 2 3 1 3 2 2 3 2 1 3 2 2 2 2 2 4 3 2 3 2
    ##  [297] 3 2 2 1 1 3 1 3 2 3 3 2 2 1 3 3 2 2 3 1 1 4 2 2 2 2 4 2 3 2 3 2 2 1 2 1 2
    ##  [334] 3 2 1 2 3 3 2 1 2 1 2 3 2 1 2 3 2 4 2 2 3 2 1 1 4 2 2 1 2 4 4 2 1 1 3 2 4
    ##  [371] 1 1 3 1 3 3 2 1 2 2 4 2 2 2 1 2 2 3 3 2 2 2 1 2 2 2 2 2 1 2 2 2 2 1 3 1 1
    ##  [408] 3 2 3 1 2 1 2 4 2 3 2 1 2 2 1 2 1 3 3 1 2 4 1 1 1 1 1 2 2 3 1 1 1 2 1 2 2
    ##  [445] 1 2 4 1 4 1 3 1 3 2 3 2 4 2 1 2 1 2 2 3 2 2 1 2 3 3 1 3 2 2 2 3 1 1 1 4 2
    ##  [482] 1 2 2 2 2 1 1 2 3 3 1 2 1 2 1 1 3 4 3 3 3 2 3 3 2 2 4 2 3 2 2 2 3 1 2 1 1
    ##  [519] 2 2 1 2 2 1 1 1 2 3 1 1 3 2 3 1 2 1 1 2 1 1 2 2 3 2 2 1 1 1 1 2 2 2 2 1 3
    ##  [556] 2 1 1 3 3 2 4 2 3 2 3 2 1 3 1 1 4 1 2 2 2 4 1 1 1 2 1 2 3 2 2 2 1 2 2 4 2
    ##  [593] 2 3 2 2 2 4 1 2 2 1 2 1 2 3 1 3 1 1 1 1 1 2 3 2 1 2 3 1 3 2 2 3 3 1 1 2 3
    ##  [630] 1 3 4 1 1 4 1 1 2 1 4 1 3 1 2 2 3 3 1 2 2 2 2 4 1 2 2 4 1 1 2 2 4 4 1 1 3
    ##  [667] 1 1 2 2 4 2 1 4 1 1 1 2 3 1 2 2 1 1 2 1 3 2 2 1 1 3 3 3 1 1 3 3 3 4 1 1 2
    ##  [704] 2 2 2 2 1 1 3 1 1 1 2 1 4 2 3 1 1 2 2 1 4 2 1 4 1 3 2 2 1 2 1 2 2 2 2 4 1
    ##  [741] 2 1 1 1 3 1 2 2 3 1 1 2 2 1 1 2 2 2 1 2 2 2 2 1 4 1 2 2 2 1 1 3 1 2 1 1 1
    ##  [778] 2 1 4 2 4 2 2 2 2 2 4 2 1 4 1 2 3 1 1 2 2 4 2 2 1 1 3 2 2 4 4 4 2 3 2 2 3
    ##  [815] 2 2 1 1 1 3 3 2 3 2 2 1 1 3 1 3 2 2 3 1 2 3 2 1 1 1 1 4 3 1 1 4 3 2 2 2 2
    ##  [852] 3 3 1 2 3 1 1 3 2 1 2 3 1 1 2 4 1 4 2 3 1 2 3 3 1 1 3 3 3 2 2 1 2 2 2 1 1
    ##  [889] 2 1 2 1 1 2 1 4 3 1 4 1 1 1 2 3 3 1 2 1 2 1 3 1 1 2 2 4 4 1 2 2 2 1 2 1 1
    ##  [926] 2 2 3 1 1 4 1 2 1 2 1 2 1 2 3 1 2 2 2 2 2 2 1 2 3 3 3 2 3 1 2 1 2 2 2 1 2
    ##  [963] 3 2 2 4 1 2 3 3 2 3 1 3 1 4 3 4 3 2 1 3 2 4 2 4 1 2 3 2 2 3 1 2 1 3 1 1 1
    ## [1000] 2 1 2 4 3 4 1 1 1 2 2 2 1 2 2 2 2 1 2 2 2 1 1 1 2 2 2 1 2 1 2 2 1 2 3 2 3
    ## [1037] 2 4 2 1 1 2 2 1 1 3 1 3 3 2 2 2 3 2 4 3 3 3 1 3 1 3 2 3 1 2 4 2 1 3 1 3 4
    ## [1074] 1 1 1 4 2 2 3 2 3 1 1 1 3 4 1 1 2 2 1 2 3 1 3 2 2 2 4 2 1 1 3 3 2 2 1 2 1
    ## [1111] 2 1 4 2 2 2 2 1 1 3 3 2 3 1 3 1 3 1 1 3 1 2 2 2 4 4 3 1 1 1 1 2 2 4 1 4 1
    ## [1148] 2 2 1 1
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 466476.7 844236.2 399180.9 504226.0
    ##  (between_SS / total_SS =  71.9 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

Les informations ci-dessus nous donne accès aux barycentres de chaque
variable, l’assignation de chaque individus à son cluster et la
précision de notre clustering en regardant le ratio inter. Ce ratio
inter est de 71,9%, ce qu’on peut considérer comme un résultat correct.

Car le KMeans a ses limites et il est possible que dans notre dataset,
il n’y est pas de groupes distincts qui se déssinent. Afin de vérifier
cela nous pouvons afficher graphiquement les résultats de notre
clustering.

Cependant celui-ci peut être difficile en raison de la nature
dimensionnelle élevée des données. Pour surmonter ce problème, la
focntion plot effectue une mise à l’échelle multidimensionnelle pour
projeter les données en deux dimensions. Elle utilise l’agorithme des
PCA, et affiche sur les deux composantes principales les plus
importantes.

``` r
#Graphique du résultat du KMeans 
plot(km_data, data = data, class = "V20")
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->
Ce graphique nous aide à voir qu’il semble compliqué de séparer les
classes. Et que nos clusters possèdent des individus des deux classes.

Nous pouvons extraire le profil des clusters grâce à une matrice de
confusion.

``` r
#Matrice de confusion pour notre clustering
plot(table(data$V20, km_data$cluster), main="Matrice de confusion", xlab = "V20", ylab = "Clusters")
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->
On remarque que nos clusters 1 et 2 possèdent plus d’individus que les
clusters 3 et 4. Le clusters 4 possèdent peu d’individus négatifs. Il
semblerait que ce clusters représente le mieux les individus positifs.
Cependant il faut prendre en compte qu’elle reprèsente un total
d’individus assez faible. Et que les clusters 1 et 2 représentes de
nombreux individus positifs. Nous pouvons donc en conclure qu’il est
difficile de reproduire la distinction entre individus malades et sains
avec l’algorithme des KMeans.

On peut aussi réaliser un autre KMeans, avec seulement 2 clusters,
puisque nous savons que notre variable cible classifie nos patients en
deux catégories : malade ou sain. On analyse ensuite avec ce même
graphique l’éfficacité de notre algorithme.

``` r
#Test avec deux clusters pour reproduire notre variable "target"
km_data2 <- kmeans(unsu_data, centers = 2)
km_data2
```

    ## K-means clustering with 2 clusters of sizes 832, 319
    ## 
    ## Cluster means:
    ##          V1        V2       V3       V4       V5       V6       V7       V8
    ## 1 0.9951923 0.9122596 43.58534 41.99519 40.38942 37.42548 33.83413 25.19111
    ## 2 1.0000000 0.9341693 24.97806 23.64577 21.45141 18.92163 15.47962 10.61442
    ##          V9      V10       V11       V12       V13       V14        V15
    ## 1  34.62441 14.55230  4.320277 0.9640846 0.3627768 0.1461336 0.06539487
    ## 2 140.96477 45.35042 20.139611 4.1118513 1.0770510 0.3848354 0.13856552
    ##          V16       V17       V18       V19
    ## 1 0.03212265 0.5244957 0.1091665 0.2127404
    ## 2 0.05053320 0.5198648 0.1065138 0.6583072
    ## 
    ## Clustering vector:
    ##    [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 2 1 2 1 2 2 1 1 2 1 1 1 1 2 2 1 1 1
    ##   [38] 1 1 1 1 2 2 1 2 1 1 1 1 2 1 1 1 1 1 1 1 2 1 2 1 2 1 2 2 1 1 1 2 1 1 1 1 1
    ##   [75] 1 1 1 1 1 1 1 1 1 2 1 1 2 1 1 1 1 1 1 2 2 2 1 2 1 2 1 1 2 2 2 1 1 1 1 1 1
    ##  [112] 1 1 2 1 1 1 1 2 1 2 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 2 1 1 1 2 1 1 1 1 1
    ##  [149] 1 2 1 1 1 1 1 1 2 1 1 1 1 1 2 1 1 1 1 1 2 1 1 1 1 1 1 1 1 2 1 1 1 1 2 1 1
    ##  [186] 2 1 1 1 1 1 2 1 1 1 2 1 1 2 1 1 1 2 2 1 1 1 1 1 1 2 1 2 2 1 2 1 2 1 1 1 1
    ##  [223] 1 2 1 2 1 1 1 1 1 1 2 1 1 2 1 1 1 2 1 2 1 1 1 2 1 1 1 1 1 1 1 2 1 2 1 1 1
    ##  [260] 2 1 1 1 1 2 1 1 1 2 1 1 1 2 1 1 1 1 2 1 2 1 1 2 1 1 2 1 2 1 1 1 2 2 1 2 1
    ##  [297] 2 1 1 1 2 2 1 2 1 2 2 1 1 1 2 2 1 1 2 1 1 2 1 1 1 1 2 1 2 1 2 1 1 1 1 1 1
    ##  [334] 2 1 2 1 2 2 1 1 1 2 1 2 1 1 1 2 1 2 1 1 2 1 1 1 2 1 1 1 1 2 2 1 1 1 2 1 2
    ##  [371] 1 1 2 1 2 2 1 1 1 1 2 1 1 1 1 1 1 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ##  [408] 2 1 2 1 1 1 1 2 1 2 1 1 1 1 1 1 1 2 2 1 1 2 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1
    ##  [445] 1 1 2 1 2 1 2 1 2 1 1 1 2 1 1 1 1 1 1 2 1 1 1 1 2 2 1 2 1 1 1 2 1 1 1 2 1
    ##  [482] 1 1 1 1 1 1 1 1 2 2 1 1 1 1 1 1 2 2 2 2 2 1 2 2 1 1 2 1 2 1 1 1 2 1 1 1 1
    ##  [519] 1 1 1 1 1 1 1 1 1 2 1 1 2 1 2 2 1 1 1 1 2 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 2
    ##  [556] 1 1 1 2 2 1 2 1 2 1 2 1 1 2 1 1 2 1 1 1 1 2 1 2 1 1 1 1 2 1 1 1 1 1 1 2 1
    ##  [593] 1 2 1 1 1 2 1 1 1 1 1 1 1 2 1 2 1 1 1 1 1 1 2 1 1 1 2 1 2 1 1 2 2 1 1 1 2
    ##  [630] 1 2 2 1 1 2 1 1 1 1 2 1 2 1 1 1 2 2 2 1 1 1 1 2 1 1 1 2 1 1 1 1 2 2 1 1 2
    ##  [667] 1 1 1 1 2 1 1 2 2 1 1 1 2 2 1 1 1 1 1 1 2 1 1 1 1 2 2 2 1 1 2 2 2 2 1 1 1
    ##  [704] 1 1 1 1 1 1 2 1 1 2 1 1 2 1 1 2 1 1 1 1 2 1 1 2 1 2 1 1 1 1 1 1 1 1 1 2 1
    ##  [741] 1 1 1 2 2 2 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 2 1 1 1 1 1
    ##  [778] 1 1 2 1 2 1 1 1 1 1 2 1 1 2 1 1 2 1 1 1 1 2 1 1 1 1 2 1 1 2 2 2 1 2 1 1 2
    ##  [815] 1 1 1 1 1 2 2 1 2 1 1 1 1 2 1 2 1 1 2 1 1 2 1 1 1 1 1 2 2 1 1 2 2 1 1 1 1
    ##  [852] 2 2 1 1 2 1 1 2 1 1 1 2 1 1 1 2 1 2 1 2 1 1 2 2 1 1 2 2 2 1 1 1 1 1 1 2 1
    ##  [889] 1 1 1 1 1 1 1 2 2 1 2 1 1 1 1 2 2 1 1 1 1 1 2 1 1 1 1 2 2 1 1 1 1 1 1 1 1
    ##  [926] 1 1 2 2 1 2 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 2 2 2 1 2 1 1 1 1 1 1 1 1
    ##  [963] 2 1 1 2 1 1 2 2 1 2 1 2 1 2 2 2 2 1 1 2 1 2 1 2 2 1 2 1 1 2 1 1 1 2 1 1 1
    ## [1000] 1 1 1 2 2 2 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1
    ## [1037] 1 2 1 1 1 1 1 1 1 2 1 2 2 1 1 1 2 1 2 2 1 2 1 2 1 2 1 2 1 1 2 1 1 2 1 2 2
    ## [1074] 2 1 1 2 1 1 2 2 2 1 1 1 2 2 1 1 1 1 1 1 2 1 2 1 1 1 2 1 1 1 2 2 1 1 2 1 1
    ## [1111] 1 1 2 1 1 1 1 1 1 2 2 1 2 1 2 1 2 1 1 2 1 1 1 1 2 2 2 1 1 1 1 1 1 2 1 2 1
    ## [1148] 1 1 1 1
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 2843451 1709021
    ##  (between_SS / total_SS =  42.3 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
plot(km_data2, data = data, class = "V20")
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->
Les résultats obtenus ne sont pas bon, puisque nous avons seulement un
résultat de 42,3% pour le ratio inter. De plus si on regarde le
graphique obtenu, on peut voir qu’il y a une faible corrélation entre
les couleurs (signification de notre clustering) et les formes
(signification des classes réelles), ce qui nous indique un mauvais
clustering.

Afin de vérifier cela nous utilisons une matrice de confusion.

``` r
plot(table(data$V20, km_data2$cluster), main="Matrice de confusion", xlab = "V20", ylab = "Clusters")
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
#La classe 0 correspond à la 1 et la 1 à la 2 pour la variable V20
```

Cette matrice nous montre que notre cluster 1 correspond trop peu
d’élément de la classe 0 (équivalent au cluster 1). Si les résultats
étaient bon, la diagonale de la matrice aurait de fortes valeurs.

L’un des problème pour le clustering avec l’algorithme KMeans est qui
est sensible au outliers (valeurs abérantes). Et lors de notre analyse
descriptive, nous avons grâce aux boxplots, que notre jeu de donné
possède des outliers.

Une alternative à ce problème est les K-Medoïds. Au lieu que le centre
d’un cluster soit la moyenne du cluster, le centre est l’une des
observations réelles du cluster. Cela s’apparente à la médiane, qui est
robuste contre les valeurs aberrantes. L’algorithme le plus connu des
K-Medoïds est Partitionning Around Medoids (PAM).

``` r
#On importe la librairie permettant d'utiliser PAM
library(cluster)
#On passe nos data dans la matrice de distance entre les individus
X <- dist(unsu_data)
#Utilisation de l'algorithme PAM sur la matrice de distance
#On garde le même nombre de cluster que pour le kmeans
pam_data <- pam(X, k = 4)
pam_data
```

    ## Medoids:
    ##        ID     
    ## [1,] 1022 1022
    ## [2,]  953  953
    ## [3,] 1051 1051
    ## [4,] 1120 1120
    ## Clustering vector:
    ##    [1] 1 1 2 2 3 3 3 1 1 2 2 1 2 3 2 1 2 3 2 4 3 4 3 1 4 1 2 4 3 2 1 3 4 4 2 3 1
    ##   [38] 2 3 1 2 1 4 1 1 3 2 1 4 1 1 3 1 1 3 2 1 4 3 4 1 4 1 4 2 1 2 2 4 2 2 1 2 2
    ##   [75] 2 3 3 1 1 1 2 1 2 4 3 3 4 3 1 1 1 2 2 1 4 4 2 4 3 4 3 3 4 1 4 3 4 1 1 3 3
    ##  [112] 3 2 4 2 1 3 3 4 2 1 1 3 3 1 1 3 2 2 2 3 1 2 2 3 1 3 2 1 1 2 1 4 3 4 2 1 2
    ##  [149] 3 4 3 1 2 1 1 3 4 3 3 2 3 3 4 1 2 1 1 2 1 1 1 3 3 1 1 1 2 1 2 2 3 1 4 3 2
    ##  [186] 4 2 3 2 3 2 4 3 3 2 1 2 3 4 1 1 3 4 4 1 2 1 1 2 3 1 2 4 4 1 4 2 4 2 3 2 1
    ##  [223] 1 4 2 4 1 2 1 2 1 1 4 3 2 1 1 1 1 4 3 4 2 1 1 4 1 1 2 3 3 2 3 1 3 4 2 1 1
    ##  [260] 4 1 3 3 1 1 2 1 3 4 2 1 2 4 2 1 2 3 4 1 4 2 2 4 2 1 1 2 2 2 2 2 4 4 3 4 2
    ##  [297] 4 2 3 1 1 4 1 1 2 4 4 2 2 1 4 4 2 3 4 1 1 4 2 3 3 2 4 3 4 3 4 3 3 3 2 1 2
    ##  [334] 4 2 1 2 4 4 2 1 2 1 3 4 2 1 2 4 2 4 2 3 4 3 1 1 4 3 2 3 2 4 4 3 1 1 4 3 4
    ##  [371] 1 3 4 3 4 4 2 3 2 3 4 3 2 3 3 3 2 4 4 3 2 3 3 2 2 2 2 2 1 2 2 3 3 1 1 1 1
    ##  [408] 4 2 4 1 2 1 2 4 3 4 2 1 2 2 3 3 3 4 4 1 2 4 1 3 1 1 1 2 2 4 1 1 1 2 1 2 2
    ##  [445] 1 3 4 1 4 1 4 1 4 3 3 2 4 3 1 2 3 2 3 4 2 2 1 2 1 4 1 4 2 2 2 4 1 1 3 4 3
    ##  [482] 3 3 2 2 2 1 1 2 4 4 3 3 1 3 1 1 4 4 4 4 4 2 4 4 2 2 4 2 4 3 2 2 4 1 3 3 3
    ##  [519] 3 3 1 2 2 1 1 1 2 1 3 1 4 2 4 1 2 1 1 2 1 1 3 3 4 2 3 3 1 1 1 3 2 2 2 1 4
    ##  [556] 3 3 1 4 4 2 4 2 4 2 1 2 1 4 1 1 4 3 3 3 2 4 1 1 1 3 1 3 4 3 2 3 1 2 2 4 2
    ##  [593] 3 4 2 2 2 4 1 3 2 1 3 1 2 4 1 4 1 1 1 3 1 3 4 2 3 2 4 3 4 2 2 4 4 1 1 2 4
    ##  [630] 3 4 4 1 1 4 3 3 3 1 4 1 4 1 2 2 4 4 1 2 2 2 3 4 1 2 2 4 3 3 3 2 4 4 1 1 4
    ##  [667] 1 3 3 2 4 3 1 4 1 1 1 2 4 1 2 2 3 1 3 1 4 2 3 1 1 4 4 4 3 1 1 4 4 4 1 1 3
    ##  [704] 3 2 2 2 1 3 4 3 1 1 2 3 4 3 4 1 3 3 3 1 4 3 1 4 1 4 2 2 3 2 1 2 3 2 3 4 1
    ##  [741] 3 1 3 1 4 1 2 3 4 1 1 2 2 3 1 3 3 2 1 3 2 3 2 3 4 3 2 3 3 1 1 4 1 2 1 1 1
    ##  [778] 2 1 4 2 4 3 3 3 3 3 4 2 1 4 1 2 4 1 3 2 2 4 2 2 1 1 4 2 3 4 4 4 2 4 2 2 4
    ##  [815] 2 2 1 1 1 4 4 3 4 2 2 1 3 4 1 4 2 3 4 1 2 4 3 1 3 3 1 4 4 1 3 4 4 2 2 2 2
    ##  [852] 4 4 3 2 4 1 1 4 2 1 3 4 1 3 2 4 1 4 3 4 1 2 4 4 3 1 4 4 4 2 2 1 2 3 3 1 1
    ##  [889] 3 1 3 1 1 2 3 4 1 1 4 3 3 1 3 1 4 3 2 1 2 1 4 3 1 2 2 4 4 1 2 3 3 1 3 1 3
    ##  [926] 3 3 4 1 1 4 1 3 1 2 1 2 1 3 4 1 2 2 3 2 2 2 1 2 4 4 1 2 4 3 2 1 2 3 3 1 2
    ##  [963] 4 2 3 4 1 2 4 4 2 4 1 4 3 4 4 4 2 2 3 4 3 4 2 4 1 2 4 2 2 4 3 3 1 4 1 1 1
    ## [1000] 2 1 2 4 1 4 1 1 3 2 3 3 1 3 2 2 2 1 2 2 2 1 1 1 3 2 2 1 2 1 2 2 1 3 4 3 4
    ## [1037] 2 4 2 1 1 2 3 1 1 4 1 4 4 2 3 3 4 3 4 4 3 4 1 1 3 4 3 4 1 3 4 2 3 4 1 4 4
    ## [1074] 1 1 3 4 2 2 4 2 4 1 1 1 4 4 3 1 3 3 1 2 4 3 4 2 2 2 4 3 1 1 4 4 3 2 1 2 3
    ## [1111] 2 3 4 2 2 2 2 3 1 4 4 2 4 1 4 1 4 1 1 4 3 3 2 3 4 4 4 3 1 1 3 3 3 4 3 4 3
    ## [1148] 2 3 1 1
    ## Objective function:
    ##    build     swap 
    ## 39.70313 37.71182 
    ## 
    ## Available components:
    ## [1] "medoids"    "id.med"     "clustering" "objective"  "isolation" 
    ## [6] "clusinfo"   "silinfo"    "diss"       "call"

On obtient le résultat ci-dessus, nous nous intéressons au résultat du
build et du swap de la fonction objective. On remarque, que nos
résultats ne s’améliorent que très peu après la deuxième étape de
l’algorithme (le swap).

On va maintenant observer la silhoutte de notre clustering, ce qui nous
donnera plus d’information sur l’éfficacité de notre algorithme.

``` r
#Afin d'afficher graphiquement le résultat, on utilise la fonction silhoutte
sil <- silhouette(pam_data)
#On met le paramètre border à NA afin d'éviter des problèmes d'affichage du graphique
plot(sil, main = "", border = NA)
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->
Nous savons que plus l’average width est élevée, plus notre clustering
est bon. Or dans notre cas, elle est de 0.33. Un consensus scientifique
autour de la valeur silhouette nous dit qu’une valeur de 0.33 correspond
à une structure faible, presque une structure artificielle.

Il nous faut donc utiliser une autre méthode. Il est possible que nos
classes ne soient pas linéairement séparable.

On utilise donc une dernière méthode de clustering. La classification
hiérachique ascendante (CAH). Cette méthode construit des clusters dans
des clusters. Elle ne requière pas d’un nombre initial de clusters.
Cette méthode peut être vue comme un arbre, représenté graphiquement par
un dendrogramme.

``` r
#On utilise la fonction hclust pour utiliser l'algorithme de la CAH
hc_data <- hclust(X, method = "ward.D")
plot(hc_data, labels = FALSE)
#Séparation en 4 clusters
rect.hclust(hc_data, k = 4, border = "red")
#Séparation en 8 clusters
rect.hclust(hc_data, k = 8, border = "blue")
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->
On observe que avec 4 clusters, chacun d’entre eux possèdent un poids
élevé. Afin de réalisé un test nous avons aussi regardé avec 8 clusters.
Le poids semble être divisé par deux, mais pour cela nous avons du
doubler le nombre de clusters. Cela n’est pas donc pas intéressant dans
notre cas. Nos 4 clusters on un poids très élevé, ce qui ne permet pas
de dire que nous avons un bon clustering. Peut importe la méthode nos
résultats sont mauvais.

On en déduit que la classification non supervisé ne marche pas. Cela
peut s’expliquer par le fait que certaines variables n’ont pas de
rapport avec le phénomène observé (ici la variable V20). Ces variables
biaisent donc nos résultats.

Cependant, cela ne sera pas le cas avec un arbre de décision, qui lui
regarde variable par variable. Ce qui nous amène au point suivant.

# Classification supervisée

Dans cette partie nous allons procéder à l’apprentissage d’un arbre de
classification pour la prédiction de la variable classe en fonction des
variables restantes.

Nous allons utiliser un protocole d’apprentissage par Booststrap,
indiquer les performances de l’arbre appris, visualiser et interpréter
l’arbre induit en analysant les principales règles de décision.

On relie nos données.

``` r
#On récupère nos données depuis le fichier texte
data = read.table("messidor_features.txt", header = FALSE, sep = ",")
#On définit la varibale 20 comme une variable catégorielle
data$V20 = as.factor(data$V20)
head(data)
```

    ##   V1 V2 V3 V4 V5 V6 V7 V8       V9       V10       V11      V12      V13
    ## 1  1  1 22 22 22 19 18 14 49.89576 17.775994  5.270920 0.771761 0.018632
    ## 2  1  1 24 24 22 18 16 13 57.70994 23.799994  3.325423 0.234185 0.003903
    ## 3  1  1 62 60 59 54 47 33 55.83144 27.993933 12.687485 4.852282 1.393889
    ## 4  1  1 55 53 53 50 43 31 40.46723 18.445954  9.118901 3.079428 0.840261
    ## 5  1  1 44 44 44 41 39 27 18.02625  8.570709  0.410381 0.000000 0.000000
    ## 6  1  1 44 43 41 41 37 29 28.35640  6.935636  2.305771 0.323724 0.000000
    ##        V14      V15      V16      V17      V18 V19 V20
    ## 1 0.006864 0.003923 0.003923 0.486903 0.100025   1   0
    ## 2 0.003903 0.003903 0.003903 0.520908 0.144414   0   0
    ## 3 0.373252 0.041817 0.007744 0.530904 0.128548   0   1
    ## 4 0.272434 0.007653 0.001531 0.483284 0.114790   0   0
    ## 5 0.000000 0.000000 0.000000 0.475935 0.123572   0   1
    ## 6 0.000000 0.000000 0.000000 0.502831 0.126741   0   1

Maintenant que nous avons récupéré nos données, nous pouvons réaliser
notre apprentissage.

Pour réaliser un apprentissage supervisé, nous allons séparer notre
dataset en un trainset et un testset, dans le but de vérifier les
résultats de notre algorithme.

``` r
#Taille du train
smp_size <- floor(0.8 * nrow(data))

#On donne seed pour obtenir tout le temps la même partition
set.seed(12)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

#Split en trainset et testset
trainset <- data[train_ind, ]
testset <- data[-train_ind, ]
```

Maintenant que nous avons notre testset et trainset, nous allons
préparer nos dataframes pour les utiliser dans un algortihme d’arbre de
décision et xgboost.

Nous allons commencer par un algorithm de decision tree.

``` r
#Import des librairies utiles pour faire un arbre
library(rpart)
library(rpart.plot)
```

    ## Warning: le package 'rpart.plot' a été compilé avec la version R 4.1.2

``` r
#Définition de l'arbre et affichage de celui-ci
fit <- rpart(trainset$V20~., data = trainset)
rpart.plot(fit)
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->
Grâce à cette arbre nous pouvons visualiser les principales règles. On
peut voir sur chaque feuilles la classe majoritaire de la feuille (0 ou
1), ainsi que le pourcentage d’individus total de la feuille. On
remarque que nous avons 7 règles de décisions, et que nous avons donc 8
feuilles à notre arbre. Dont 5 correspondants à la classe 1 (individus
malade) et 3 correspondants à la classe 0 (individus sain).

Une autre visualisation est possible, moins complète, mais qui met plus
en valeur les régles de l’arbre et les classes obtenues suite a ces
régles.

``` r
#On peut réaliser une autre méthode d'affichage
plot(fit)
text(fit)
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->
De plus les règles sont données avec une plus grande précision après la
virgule. On peut maintenant afficher notre arbre sans utiliser de
graphique, afin d’avoir plus d’information sur les règles obtenus.

``` r
#Ici on affiche les règles de notre arbre, la classe majoritaire de chaque régles
fit
```

    ## n= 920 
    ## 
    ## node), split, n, loss, yval, (yprob)
    ##       * denotes terminal node
    ## 
    ##  1) root 920 442 1 (0.4804348 0.5195652)  
    ##    2) V3< 55.5 694 302 0 (0.5648415 0.4351585)  
    ##      4) V15< 0.061455 637 256 0 (0.5981162 0.4018838)  
    ##        8) V9< 127.8097 540 198 0 (0.6333333 0.3666667)  
    ##         16) V12>=0.0405425 381 119 0 (0.6876640 0.3123360) *
    ##         17) V12< 0.0405425 159  79 0 (0.5031447 0.4968553)  
    ##           34) V18>=0.0868095 150  70 0 (0.5333333 0.4666667)  
    ##             68) V13< 0.0019225 136  59 0 (0.5661765 0.4338235) *
    ##             69) V13>=0.0019225 14   3 1 (0.2142857 0.7857143) *
    ##           35) V18< 0.0868095 9   0 1 (0.0000000 1.0000000) *
    ##        9) V9>=127.8097 97  39 1 (0.4020619 0.5979381)  
    ##         18) V4< 4.5 7   0 0 (1.0000000 0.0000000) *
    ##         19) V4>=4.5 90  32 1 (0.3555556 0.6444444) *
    ##      5) V15>=0.061455 57  11 1 (0.1929825 0.8070175) *
    ##    3) V3>=55.5 226  50 1 (0.2212389 0.7787611) *

On remaque les feuilles obtenus sur les 920 individus utilisés pour
notre apprentissage, certaines ont très bien classées les individus, et
la classe majortiaire de la feuille est supérieur à 70%. Alors que
certaines feuilles ont moins bien classés les individus.

Cette affichage nous montre : la règle, le nombre d’individus de la
classe majoritaire, puis le nombre d’individus de la classe minoritaire.
Enfin nous avons la probabilité d’appartenance à une classe. Par exemple
pour la règle 3 qui nous dit que la variable V3 doit être supérieur à
55.5, nous avons une probabilité de 78% d’être dans la classe 1.

On peut maintenant s’intéresser à la performance de notre arbre. Pour
cela nous utilisons une matrice de confusion.

``` r
#Pour calculer la performance de notre arbre nous allons utiliser une matrice de confusion
library(caret)
```

    ## Warning: le package 'caret' a été compilé avec la version R 4.1.3

    ## Le chargement a nécessité le package : lattice

``` r
ypred <- predict(fit, testset, type = "class")
result <- confusionMatrix(ypred, testset$V20)
result
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  0  1
    ##          0 71 53
    ##          1 27 80
    ##                                           
    ##                Accuracy : 0.6537          
    ##                  95% CI : (0.5885, 0.7149)
    ##     No Information Rate : 0.5758          
    ##     P-Value [Acc > NIR] : 0.009414        
    ##                                           
    ##                   Kappa : 0.315           
    ##                                           
    ##  Mcnemar's Test P-Value : 0.005189        
    ##                                           
    ##             Sensitivity : 0.7245          
    ##             Specificity : 0.6015          
    ##          Pos Pred Value : 0.5726          
    ##          Neg Pred Value : 0.7477          
    ##              Prevalence : 0.4242          
    ##          Detection Rate : 0.3074          
    ##    Detection Prevalence : 0.5368          
    ##       Balanced Accuracy : 0.6630          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

On observe que nous avons une précision de 65%, ce qui n’est pas très
élevé. On peut donc utiliser une méthode par boostrap afin d’améliorer
nos résulats.

``` r
#On définie nos vecteurs de variables train et test et nos vecteurs de variable target train et test
#On convertie nos dataframe en matrice car xgboost ne prend en entrée que des matrices
X_train <- as.matrix(trainset[-20])
y_train <- as.matrix(trainset[20])
X_test <- as.matrix(testset[-20])
y_test <- as.matrix(testset[20])
```

Maintenant que nous avons nos matrices, contenant nos variables dans un
première matrice et notre variables target dans une seconde. Nous
pouvons utiliser l’algorithme xgboost afin de réaliser un arbre de
décision par boostrap.

On règle les paramètres suivants : eta contrôle le taux d’apprentissage,
il échelonne la contribution de chaque arbre. Utilisé pour éviter
l’overfitting en rendant le processus de boosting plus conservateur. Une
valeur plus faible pour eta implique une valeur plus grande pour
nrounds. Une valeur eta faible signifie un modèle plus robuste au
surajustement mais plus lent à calculer. nrounds nous permet de fixer le
nombre d’itération que l’agorithme fait. Enfin max.depth nous permet de
contrôler la profondeur maximale de l’arbre.

En jouant sur ces hyperparamètre, nous pouvons améliorer les
performances de notre algorithme.

``` r
#XGBOOST
library(xgboost)
```

    ## Warning: le package 'xgboost' a été compilé avec la version R 4.1.2

``` r
boostedtree <- xgboost(data = X_train, label = y_train, eta = .3, nrounds = 25, max.depth = 8, objective = 'binary:logistic')
```

    ## [10:43:06] WARNING: amalgamation/../src/learner.cc:1115: Starting in XGBoost 1.3.0, the default evaluation metric used with the objective 'binary:logistic' was changed from 'error' to 'logloss'. Explicitly set eval_metric if you'd like to restore the old behavior.
    ## [1]  train-logloss:0.590588 
    ## [2]  train-logloss:0.522922 
    ## [3]  train-logloss:0.477219 
    ## [4]  train-logloss:0.442125 
    ## [5]  train-logloss:0.404100 
    ## [6]  train-logloss:0.355632 
    ## [7]  train-logloss:0.328079 
    ## [8]  train-logloss:0.310761 
    ## [9]  train-logloss:0.287194 
    ## [10] train-logloss:0.275922 
    ## [11] train-logloss:0.253527 
    ## [12] train-logloss:0.245665 
    ## [13] train-logloss:0.219523 
    ## [14] train-logloss:0.212676 
    ## [15] train-logloss:0.198613 
    ## [16] train-logloss:0.191969 
    ## [17] train-logloss:0.187285 
    ## [18] train-logloss:0.178639 
    ## [19] train-logloss:0.166064 
    ## [20] train-logloss:0.152807 
    ## [21] train-logloss:0.145867 
    ## [22] train-logloss:0.139273 
    ## [23] train-logloss:0.134776 
    ## [24] train-logloss:0.131575 
    ## [25] train-logloss:0.127601

On utilisant l’algorithme de xgboost, nous arrivons au bout de 25
iterations à grandement améliorer les résulats de notre apprentissage.
Nous arrivons à une erreur moyenne de 12% seulement. Ce qui représente
de bon résultat.

On peut donc maintenant regarder les variables les plus importantes lors
de l’apprentissage de notre arbre. Afin de comprendre quelles variables
sont les plus utiles dans l’apprentissage de l’arbre.

``` r
#Affichage de l'importance des variables
library(DiagrammeR)
```

    ## Warning: le package 'DiagrammeR' a été compilé avec la version R 4.1.2

``` r
xgb.plot.importance(xgb.importance(boostedtree, feature_names = colnames(X_train)))
```

![](Diabetic_Retinopathy_Debrecen_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->
On voit que la variable 3 est la plus importante. La variable 15 et 9 le
sont aussi.

``` r
#Prediction du modèle sur le jeu de test
y_pred <- predict(boostedtree, X_test, type = "class")

#Convertion du vecteur de probabilité en classe 1 ou 0
y_pred <- as.numeric(y_pred > 0.5)

#Erreur moyenne sur le jeu de test
err <- mean(y_pred != y_test)
print(paste("test-error=", err))
```

    ## [1] "test-error= 0.25974025974026"

``` r
#Matrice de confusion
result <- confusionMatrix(factor(y_pred), factor(y_test))
result
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  0  1
    ##          0 75 37
    ##          1 23 96
    ##                                           
    ##                Accuracy : 0.7403          
    ##                  95% CI : (0.6787, 0.7956)
    ##     No Information Rate : 0.5758          
    ##     P-Value [Acc > NIR] : 1.474e-07       
    ##                                           
    ##                   Kappa : 0.4781          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.09329         
    ##                                           
    ##             Sensitivity : 0.7653          
    ##             Specificity : 0.7218          
    ##          Pos Pred Value : 0.6696          
    ##          Neg Pred Value : 0.8067          
    ##              Prevalence : 0.4242          
    ##          Detection Rate : 0.3247          
    ##    Detection Prevalence : 0.4848          
    ##       Balanced Accuracy : 0.7436          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

On peut donc voir qu’avec XgBoost, on améliore nos résultats de presque
10%, cependant il faut faire attention à l’overfitting de notre modèle.

Lors de notre apprentissage supervisé, nous avons dû utiliser une
méthode par bosstrap afin d’améliorer nos performances. Celle-ci est
très utilisée et permet d’obtenir simplement des très bons résultats
(comme nous avons pu le voir). Elle est plus efficace qu’un simple arbre
de décision, de plus pour un problème de simple classification comme
celui-ci, il n’est pas nécessaire de se lancer dans des algorithmes de
deep learning.

# Conclusion

Pour conclure, nous avons dans ce projet une approche complète pour
traiter un jeu de données. En procédent dans un premier temps par une
analyse statistique des données, afin de mieux les comprendre, et de
mieux aborder les étapes suivantes du projet. Nous n’avons pratiquement
pas eu de pré-traitement des données à faire, ce qui m’a permis de me
concentrer sur les aspects vus en cours. Dans un deuxième temps nous
avons abordé le sujet du clustering, en supprimant notre variable
target, et en utilisant des algorithmes d’apprentissages non supervisés
comme KMeans, PAM et CAH. Malheureusement les résultats n’ont pas été
très bons. Le KMeans nous a apporté le meilleur résultat, même si
celui-ci n’est pas tout à fait convaincant, sachant que le KMeans est
sensible aux outliers, tester PAM pouvait être une solution. Cependant
les résultats sont moindres avec cet algorithme. Nous en avons conclu
que les classes sont non linéairement séparables. Ce qui rend la tâche
d’apprentissage non supervisé très compliqué. Une ouverture sur ce
problème, est le deep learning où des méthodes commencent à émerger.
Enfin dans un troisième temps, nous avons réalisé une classification
supervisée. Avec tout d’abord un arbre de décision, afin de voir les
règles obtenus par notre arbre, ainsi que la répartition de chaque
classes dans les feuilles obtenues. Les résultats n’étant pas
suffisamment efficaces, nous avons procédé à une méthode par boostrap.
Sur laquelle nous avons obtenu de meilleur résultats (89% de précision
avec 20 itérations). Ce projet m’a permis de mettre en application les
concepts vus en cours et de me familiariser avec le langage R, un
langage nouveau pour moi.
