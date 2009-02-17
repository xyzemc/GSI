#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUM_COUNTERS 6

int main(int argc, char * argv[])
{
    FILE * ifp;
    FILE * ofp;
    char app[128];
    char filename[128];
    char line[10240];
    char * ptr;
    int i, g, group, match, finished, max_group;
    int job, first_job, num_jobs = 0;
    double * elapsed, * min_comm, * med_comm, * max_comm;

    if (argc != 2) {
        printf("Syntax: reformat hpmdata.txt\n");
        exit(0);
    }

    printf("enter a descriptive application name (no spaces): \n");

    scanf("%s", app);

    strcpy(filename, app);

    strcat(filename, "_hpmdata.txt");

    ifp = fopen(argv[1], "r");

    if (ifp == NULL) {
        printf("failed to open the input file %s\n", argv[1]);
        exit(0);
    }

    ofp = fopen(filename, "w");
   
    if (ofp == NULL) {
        printf("failed to open the output file %s\n", filename);
        exit(0);
    }
   
    fprintf(ofp, ";application name;%s\n", app);

    /*-----------------------------------------------------*/
    /* first scan the file to determine the number of jobs */
    /*-----------------------------------------------------*/
    while (NULL != fgets(line, sizeof(line), ifp))
    {
        if (strncmp(line, ";system model", 13) == 0) num_jobs++;
    }
    rewind(ifp);

    elapsed  = (double *) malloc(num_jobs*sizeof(double));
    min_comm = (double *) malloc(num_jobs*sizeof(double));
    max_comm = (double *) malloc(num_jobs*sizeof(double));
    med_comm = (double *) malloc(num_jobs*sizeof(double));

    /*-------------------------------------*/
    /* save basic timing data for each job */
    /*-------------------------------------*/
    job = 0;
    while (NULL != fgets(line, sizeof(line), ifp))
    {
        if (strncmp(line, ";elapsed", 8) == 0)
        {
           ptr = strrchr(line, ';');
           if (ptr != NULL) ptr++;
           sscanf(ptr, "%lf", &elapsed[job]);

           fgets(line, sizeof(line), ifp);
           ptr = strrchr(line, ';');
           if (ptr != NULL) ptr++;
           sscanf(ptr, "%lf", &min_comm[job]);

           fgets(line, sizeof(line), ifp);
           ptr = strrchr(line, ';');
           if (ptr != NULL) ptr++;
           sscanf(ptr, "%lf", &max_comm[job]);

           fgets(line, sizeof(line), ifp);
           ptr = strrchr(line, ';');
           if (ptr != NULL) ptr++;
           sscanf(ptr, "%lf", &med_comm[job]);

           job++;
        }
    }

    rewind(ifp);

    /*----------------------------------------*/
    /* copy the first two lines of the header */
    /*----------------------------------------*/
    fgets(line, sizeof(line), ifp); fputs(line, ofp);
    fgets(line, sizeof(line), ifp); fputs(line, ofp);

    ptr = strrchr(line, ';');
    if (ptr != NULL) ptr++;

    if (strncmp(ptr, "Power5+", 7) == 0) max_group = 150;
    else                                 max_group = 132;

    /*-------------------------------------*/
    /* copy the next 6 lines of the header */
    /*-------------------------------------*/
    fgets(line, sizeof(line), ifp); fputs(line, ofp);
    fgets(line, sizeof(line), ifp); fputs(line, ofp);
    fgets(line, sizeof(line), ifp); fputs(line, ofp);
    fgets(line, sizeof(line), ifp); fputs(line, ofp);
    fgets(line, sizeof(line), ifp); fputs(line, ofp);
    fgets(line, sizeof(line), ifp); fputs(line, ofp);

    fprintf(ofp,";elapsed time (sec)");
    for (job=0; job<num_jobs; job++) fprintf(ofp,";%.3lf", elapsed[job]);
    fprintf(ofp,"\n");

    fprintf(ofp,";minimum communication time (sec)");
    for (job=0; job<num_jobs; job++) fprintf(ofp,";%.3lf", min_comm[job]);
    fprintf(ofp,"\n");

    fprintf(ofp,";maximum communication time (sec)");
    for (job=0; job<num_jobs; job++) fprintf(ofp,";%.3lf", max_comm[job]);
    fprintf(ofp,"\n");

    fprintf(ofp,";median  communication time (sec)");
    for (job=0; job<num_jobs; job++) fprintf(ofp,";%.3lf", med_comm[job]);
    fprintf(ofp,"\n");


    /*------------------------------------------------------------*/
    /* rewind and scan the input file for counter groups and data */
    /*------------------------------------------------------------*/
    rewind(ifp);

    first_job = 1;

    g = 0;

    finished = 0;

    while (finished == 0) {

       if (NULL == fgets(line, sizeof(line), ifp)) goto the_end;

       if (0 == strncmp(line, ";system model", 13))  {
          for (i=0; i<14; i++) fgets(line, sizeof(line), ifp);
          if (first_job) {
             fprintf(ofp, "\n");
             fprintf(ofp, "group;counter;counts\n");
             first_job = 0;
          }
       }
          
       sscanf(line,"%d", &group);

       if (group > max_group) break;

       if (group == g) {

           fputs(line, ofp);

           for (i=1; i<NUM_COUNTERS; i++) {

              fgets(line, sizeof(line), ifp);

              fputs(line, ofp);
           }

           if (NULL == fgets(line, sizeof(line), ifp)) finished = 1;
           else        fputs(line, ofp);
          
           g++;

           if (g > max_group) finished = 1;
       }

      else {

           while (g != group && !finished) {

               for (i=0; i<NUM_COUNTERS; i++) fprintf(ofp,"%d\n", g);

               fprintf(ofp,"\n");

               g++;

               if (g > max_group) finished = 1;
           }

           if (finished) break;

           fputs(line, ofp);

           for (i=1; i<NUM_COUNTERS; i++) {

              fgets(line, sizeof(line), ifp);

              fputs(line, ofp);
           }

           if (NULL == fgets(line, sizeof(line), ifp)) finished = 1;
           else        fputs(line, ofp);

           g++;
      }


       
    }


the_end: 

    fclose(ifp);

    fclose(ofp);

    printf("data saved to %s\n", filename);

    return 0;
    
}
