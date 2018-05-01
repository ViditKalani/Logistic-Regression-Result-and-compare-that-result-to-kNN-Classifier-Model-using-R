## Welcome to German Credit Dataset Project Webpage

Introduction:

The German Credit Dataset is a dataset that examines a German's credit worthiness and other factors associated with this worthiness. This dataset is interesting because it contains a plethora of information regarding credit worthiness. It is interesting to see the factors that are needed (and collected) to try to predict credit status. There are many entities that could benefit from analysis of this dataset. Credit Rating agencies could benefit from this dataset as they are primarily interested with evaluating a person's credit. With better prediction models, credit rating agencies could predict credit status more accurately and faster. Consumers could also stand to benefit from analysis of this dataset. With a good prediction model, a consumer could be able to predict the status of their own credit. Consumers would also learn more about what factors contribute to evaluation of credit.  With this project, we plan to obtain more information about credit rating/worthiness using outside research. Using our research and dataset, we plan to create a model to predict credit worthiness. This model will be able to predict a person's credit worthiness given important attributes of credit worthiness to a decent degree of certainty. 


### Related Work:

Hybrid models based on rough set classifiers for setting credit rating decision rules in the global banking industry:
	This article describes the importance of credit rating among credit issuers and investors. This paper also describes the problems associated with many credit rating models. In order to overcome these problems, a hybrid model was created from two existing models. 
  
Mining multiple private databases using a kNN classifier:
	This article gives insight into using the kNN classifier. This helps us to learn more about the model we plan to create. 

Using data mining to improve assessment of credit worthiness via credit scoring models:
	This article examines the history and importance of credit scoring models to the financial industry. It also examines the role of data mining to improve the assessment of credit worthiness using credit scoring models. Finally, this paper compared the accuracy of different credit rating models.  

Methods/Models:

1.	PCA Analysis
a.	[discussion and results of PCA Analysis]

2.	kNN Classifier Model
	
	Round 1:-  Duration feature is best accuracy which is 0.725.
	
	Round 2:-  Duration +  NumberPeopleMaintenance features are best accuracy which is 0.735.
	
	Round 3:-  Duration + NumberPeopleMaintenance +  Housing features are best accuracy which is 0.765.
	
	Round 4:-  Duration + NumberPeopleMaintenance + Housing + Telephone features are best accuracy which is 0.775.
	
	Round 5:-  Duration + NumberPeopleMaintenance + Housing + Telephone +  Age features are best accuracy which is 0.775.
	
	But I noticed that after Round 4 the some features accuracy going down compare to first 3 rounds.
	And In Round 5 only one features’ accuracy is going up compare to first 4 rounds.


3.	Logistic Regression Model
a.	[discussion and results of Regression Analysis]

Results and Discussion:
	[TO BE ADDED]

Conclusions:
	[TO BE ADDED]

Contributions:
	Vidit Kalani: KNN recommendation system, project set-up

  Shoun Abraham: PCA Analysis, project write-ups



References:

You-Shyang Chen, Ching-Hsue Cheng, Hybrid models based on rough set classifiers for setting credit rating decision rules in the global banking industry, Knowledge-Based Systems, Volume 39, 2013, Pages 224-239, ISSN 0950-7051, 
https://doi.org/10.1016/j.knosys.2012.11.004. 
(http://www.sciencedirect.com/science/article/pii/S0950705112003139) 
 
Bee Wah Yap, Seng Huat Ong, Nor Huselina Mohamed Husain, Using data mining to improve assessment of credit worthiness via credit scoring models, Expert Systems with Applications, Volume 38, Issue 10, 2011, Pages 13274-13283, ISSN 0957-4174, 
https://doi.org/10.1016/j.eswa.2011.04.147. 
(http://www.sciencedirect.com/science/article/pii/S0957417411006749) 
 
Xion, L., Chitti, S., & Liu, L. (n.d.). Mining multiple private databases using a kNN classifier. Retrieved March 20, 2018, from https://dl.acm.org/citation.cfm?id=1244102  

Cai, Y., Ji, D., & Cai, D. (2010, June). A KNN Research Paper Classification Method Based on Shared ... Retrieved March 20, 2018, from http://research.nii.ac.jp/ntcir/workshop/OnlineProceedings8/NTCIR/07-NTCIR8-PATMN-CaiY.pdf  

Wang, B., Liao, Q., & Zhang, C. (2013, August). Weight Based KNN Recommender System. Retrieved March 20, 2018, from http://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=6642782    
