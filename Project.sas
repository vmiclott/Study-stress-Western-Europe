PROC IMPORT DATAFILE="H:/Statistiek III/stress.csv" DBMS=CSV OUT=stress_dirty REPLACE;
	GETNAMES=YES;
RUN;
/* check if everything is in the right format */
PROC CONTENTS DATA=stress_dirty;
RUN;
/* changing formats */
DATA stress_dirty (DROP=PartnerQualityTime FamilyQualityTime ChildcareComparison HPIsPartnerOvertime CarreerYears);
	SET stress_dirty;

	PartnerQualityTime_1 = INPUT(PartnerQualityTime,BEST1.); 
	FORMAT HPPartnerQualityTime_1 BEST1.;
	FamilyQualityTime_1 = INPUT(FamilyQualityTime,BEST1.); 
	FORMAT FamilyQualityTime_1 BEST1.;
	ChildcareComparison_1 = INPUT(ChildcareComparison,BEST1.); 
	FORMAT ChildcareComparison_1 BEST1.;
	IsPartnerOvertime_1 = INPUT(IsPartnerOvertime,BEST1.); 
	FORMAT IsPartnerOvertime_1 BEST1.;
	CareerYears = CarreerYears;
RUN;

DATA stress (RENAME=(PartnerQualityTime_1=PartnerQualityTime HPIsPartnerOvertime_1=IsPartnerOvertime ChildcareComparison_1=ChildcareComparison HCFamilyQualityTime_1=FamilyQualityTime));
	SET stress_dirty;

RUN;

PROC CONTENTS DATA=stress;
TITLE 'stress dataset - structure';
RUN;

PROC PRINT DATA=stress;
TITLE 'stress dataset - data';
RUN;

/*
Outliers: Personal qualitytime in hours per week = 180uur? 24*7=168 Salary of 1EUR? Minimum wages exist in Belgium...
*/
DATA stress;
	SET stress;
	WHERE Salary > 1 & PersonalQualityTime < 180;
RUN;

/*
Univariate
*/
PROC UNIVARIATE DATA=stress;

RUN;
/*Histograms and QQplots*/
PROC UNIVARIATE DATA=stress;
	VAR Age
CareerYears
ChildcareComparison
CommutingTime
EmployedToDegree
FamilyQualityTime
JobChangeCount
Overtime
PartnerQualityTime
PersonalQualityTime
Salary
SectorChangeCount
StressDegree;
	HISTOGRAM;
	QQPLOT;
RUN;
/*Frequency-tables*/
PROC FREQ DATA=stress;
RUN;

* matrix of histograms and scatterplots ;
proc sgscatter data=stress;                                                                                                      
   * where s lt 50 and AgeCHDdiag gt 0;                                                                                           
   matrix stressdegree salary careeryears childcarecomparison /                                                                                                    
          /* group=HasChildren */
          diagonal=(histogram kernel);                                                                           
run;   
/* changing the formats of the categorical variables, using numeric labels */

/* making a library for permanent storage of formats*/
LIBNAME library 'H:\SAS\';
/* creating a format for gender */
PROC FORMAT LIBRARY = library;
VALUE $gender 'Man' = 0
			  'Vrouw' = 1; 	
RUN;
/* using the format of gender for Sex */
DATA stress;
SET stress;
FORMAT sex $gender.;
RUN;	
/* creating a format for degree */
PROC FORMAT LIBRARY = library;
VALUE $degree 'Lager onderwijs' = 1
			  'Middelbaar onderwijs' = 2
			  'Bachelor' = 3
			  'Master' = 4
			  'Doctoraat' = 5;
RUN;
/* using the format degree for HighestDegree */
DATA stress;
SET stress;
FORMAT HighestDegree $degree.;
RUN;
/* creating a format for PartOrFulltime */
PROC FORMAT LIBRARY = library;
VALUE $working_regime 'Voltijds' = 1
			  		  'Deeltijds' = 0;
RUN;
/* using the format working_regime for PartOrFulltime */
DATA stress;
SET stress;
FORMAT PartOrFulltime $working_regime.;
RUN;
/* creating a format for CommuteOption */
PROC FORMAT LIBRARY = library;
VALUE $commute_options 	'Te voet' = 1
			  			'Fiets' = 2
			 			'Openbaar vervoer' = 3
			  			'Auto (inclusief carpooling) of motorfiets' = 4;
RUN;
/* using the format commute_options for CommuteOption */
DATA stress;
SET stress;
FORMAT CommuteOption $commute_options.;
RUN;
                           
proc univariate data=stress noprint;
   class Sex  CommuteOption ;
   histogram StressDegree / midpoints=0 to 25 by 1 vscale=count vaxis=0 to 7 by 1 
                          vaxislabel='Frequency' turnvlabels  nrows=2 ncols=4  
                          cframe=ligr  cframeside=gwh cframetop=gwh cfill=gwh ;
   inset mean(4.1) n std(2.1) / noframe  position=(2,65) ;
   *format Gender $gendfmt.1 ;
   *title 'Grade Distribution for the First Chemistry Exam'<-wut?;
run;

/*
bivariate
*/

/* creating a plot to explore the linear relationship between the response variable and the main predictor variable in the model */
PROC SGPLOT DATA = stress;
LOESS X = Salary Y = StressDegree / LINEATTRS=(PATTERN=2 THICKNESS=2px COLOR= BLACK);
REG X = Salary Y = StressDegree / LINEATTRS=(PATTERN=1 THICKNESS=2px COLOR= BLACK);
LABEL Salary = "Monthly net salary (in euros)"
      StressDegree = "Stress level (Likert scale)";
TITLE "Association between stress level and salary";
RUN;

/***** simple linear regression, first model *****/
PROC REG DATA = stress;
MODEL StressDegree = Salary / CLB;
TITLE "Results of simple linear regression: stress degree and salary";
RUN;

/***** evaluating the appropriateness of the model and the underlying assumptions *****/
		/* diagnostics for the predictor variable of interest > creating a boxplot */
PROC SGPLOT DATA = stress;
VBOX salary;
LABEL Salary = "Monthly net salary (in euros)";
TITLE "Diagnostic boxplot for predictor variable of interest salary";
RUN;
		/* residual analysis */
PROC REG DATA = stress;
MODEL StressDegree = Salary / r;
TITLE "Results of simple linear regression: stress degree and salary";
RUN;
	/* checking the squared residual plot for constancy of variance */
PROC REG DATA = stress;
MODEL StressDegree = Salary;
OUTPUT OUT = residuals r = residuals student = student;
RUN;
			/* calculating the squared residuals */
DATA residuals;
SET residuals;
sq_residuals = residuals * residuals;
RUN;
			/* plotting the squared residuals versus the predictor variables */
PROC SGPLOT DATA = residuals;
SCATTER X = salary  Y = sq_residuals;
LABEL sq_residuals = "Squared residuals";
TITLE "Diagnostic plot of the squared residuals versus the predictor variable";
RUN;
	/* detecting outliers */

	/* checking normal distribution of the residuals */
PROC UNIVARIATE PLOT DATA = residuals;
VAR student;
RUN;

/***** Building the model: forward stepwise selection procedure *****/

data stress;
set stress;
if (PartnerQualityTime = .) then PartnerQualityTime = 0;
if (FamilyQualityTime = .) then FamilyQualityTime = 0;
if (ChildcareComparison = .) then ChildcareComparison = 0;
if (IsPartnerOvertime = .) then IsPartnerOvertime = 0;
run;

data stress;
set stress;
HPPartnerQualityTime = HasPartner*PartnerQualityTime;
HPIsPartnerOvertime = HasPartner*IsPartnerOvertime;
HCFamilyQualityTime = HasChildren*FamilyQualityTime;
HCChildcareComparison = HasChildren*ChildcareComparison;
run;


/* checking all the continuous variables first with Forward procedure */
PROC REG DATA = stress;
MODEL StressDegree = Salary CareerYears HasPartner HasChildren HCChildcareComparison CommutingTime EmployedToDegree FamilyQualityTime JobChangeCount Overtime HPPartnerQualityTime PersonalQualityTime SectorChangeCount HPIsPartnerOvertime / SELECTION = forward;
RUN;
/* checking all the continuous variables first with the Stepwise procedure */
PROC REG DATA = stress;
MODEL StressDegree = Salary CareerYears HasPartner HasChildren HCChildcareComparison CommutingTime EmployedToDegree FamilyQualityTime JobChangeCount Overtime HPPartnerQualityTime PersonalQualityTime SectorChangeCount HPIsPartnerOvertime / SELECTION = stepwise;
RUN;

/* alternative manual stepwise procedure to check the automatic Forward stepwise method used before and get the results for each test */
PROC REG DATA = stress;
MODEL StressDegree = Salary CareerYears;
TEST CareerYears = 0;							
RUN;

/* significant, keep in the model p = 0.0247*/
PROC REG DATA = stress;
MODEL StressDegree = Salary HCChildcareComparison HasChildren ;
TEST HCChildcareComparison = 0, HasChildren = 0;							
RUN;
PROC GLM DATA =stress;
CLASS HasChildren;
MODEL StressDegree = Salary HasChildren HCChildcareComparison;
RUN;
/* not significant */
PROC REG DATA = stress;
MODEL StressDegree = Salary CommutingTime ;
TEST CommutingTime = 0;							
RUN;
/* not significant */
PROC REG DATA = stress;
MODEL StressDegree = Salary EmployedToDegree ;
TEST EmployedToDegree = 0;							
RUN;
/* not significant */
PROC REG DATA = stress;
MODEL StressDegree = Salary HasChildren HCFamilyQualityTime ;
TEST HasChildren = 0, HCFamilyQualityTime = 0;						
RUN;
/* not significant */
PROC REG DATA = stress;
MODEL StressDegree = Salary JobChangeCount ;
TEST JobChangeCount = 0;						
RUN;
/* not significant */
PROC REG DATA = stress;
MODEL StressDegree = Salary Overtime ;
TEST Overtime = 0;								
RUN;
/* not significant */
PROC REG DATA = stress;
MODEL StressDegree = Salary HasPartner HPPartnerQualityTime ;
TEST HasPartner = 0, HPPartnerQualityTime = 0;					
RUN;
/* not significant */
PROC REG DATA = stress;
MODEL StressDegree = Salary PersonalQualityTime ;
TEST PersonalQualityTime = 0;					
RUN;
/* not significant */
PROC REG DATA = stress;
MODEL StressDegree = Salary SectorChangeCount ;
TEST SectorChangeCount = 0;						
RUN;

/* checking the categorical variables */
	/* checking the first categorical variable Sex > not significant, delete from the model */
PROC GLM DATA =stress;
CLASS Sex;
MODEL StressDegree = Salary Sex;
RUN;
/* checking the next categorical variable PartOrFulltime > not significant, delete from the model */
PROC GLM DATA =stress;
CLASS PartOrFulltime;
MODEL StressDegree = Salary PartOrFulltime;
RUN;
		/* checking the next categorical variable CommuteOption > not significant, delete from the model */
PROC GLM DATA =stress;
CLASS CommuteOption;
MODEL StressDegree = Salary CommuteOption;
RUN;
		/* checking the next categorical variable HighestDegree > not significant, delete from the model */
PROC GLM DATA =stress;
CLASS HighestDegree;
MODEL StressDegree = Salary HighestDegree;
RUN;
		/* checking the next categorical variable HasPartner > not significant, delete from the model */
PROC GLM DATA =stress;
CLASS HasPartner;
MODEL StressDegree = Salary HasPartner;
RUN;
		/* checking the next categorical variable HasChildren > not significant, delete from the model */
PROC GLM DATA =stress;
CLASS HasChildren;
MODEL StressDegree = Salary HasChildren;
RUN;
		/* checking the next categorical variable HPIsPartnerOvertime > not significant, delete from the model */
PROC GLM DATA =stress;
CLASS HPIsPartnerOvertime HasPartner;
MODEL StressDegree = Salary HasPartner HPIsPartnerOvertime;
RUN;

/*Interaction term*/
data stress;
set stress;
IntSalaryChildcare = Salary*HCChildcareComparison;
run;

/* significance-test of interactionterm (not significant)*/
PROC REG DATA = stress;
MODEL StressDegree = Salary HCChildcareComparison HasChildren IntSalaryChildcare ;
TEST IntSalaryChildcare = 0;							
RUN;

/* final model*/
PROC REG DATA = stress;
MODEL StressDegree = Salary HCChildcareComparison HasChildren ;							
RUN;
PROC GLM DATA =stress;
CLASS HasChildren;
MODEL StressDegree = Salary HasChildren HCChildcareComparison;
RUN;
/* check partial residual plot */
PROC REG DATA = stress;
MODEL StressDegree = Salary HasChildren HCChildcareComparison/ PARTIAL;
RUN;

/* Check VIF's: Pretty high (6.5 & 6.7) for HCChildcomparison & HasChildren */
/* Seems logic since HCChildcomparison also has the factor HasChildren */
/* There don't seem to be any multi-collinearity problems with Salary VIF ~ 1 */
PROC REG DATA = stress;
MODEL StressDegree = Salary HCChildcareComparison HasChildren/ VIF ;							
RUN;


/* Check assumptions */
	/* normality */
proc reg data=stress;
model StressDegree = Salary HCChildcareComparison HasChildren;							
output out=resid r=rman p=pman student=student;
plot student.*nqq.;
run;
	/* Linearity */
proc gplot data=resid;
plot rman*pman/vref=0;
run;
	/* Homoscedasticity */
data resid2;
set resid;
rman2=rman**2;
run;
proc gplot data=resid2;
plot rman2*pman;
run;

/* Check for outliers */

proc reg data=stress;
model StressDegree = Salary HCChildcareComparison HasChildren;	
run;
proc gplot data=resid;
plot student*pman;
run;

proc reg data=stress;
model StressDegree = Salary HCChildcareComparison HasChildren;	
output out=cookdis cookd=cdist;
run;
quit;
data cookdis2;
set cookdis;
n=_n_;
run;

proc gplot data=cookdis2;
plot cdist*n;
run;
