/*=====================================================================*/
/* heapsort a list into the specified order and return the index array */
/*=====================================================================*/

#define SORT_ASCENDING_ORDER 1
#define SORT_DESCENDING_ORDER -1

void mpitrace_sortx(double * list, int n, int * key, int order)
{
   int i, j, k, ind;
   int keys;
   double lists;
 
   for (i=0; i<n; i++) key[i] = i;

   if (order == SORT_DESCENDING_ORDER)
   {
       k = n/2;
       ind = n - 1;
       while (n > 1) 
       {
           if (k > 0)
           {
               k = k - 1;
               lists = list[k];
               keys = key[k];
           }
           else 
           {
               lists = list[ind];
               keys = key[ind];
               list[ind] = list[0];
               key[ind] = key[0];
               ind = ind - 1;
               if (ind <= 0)
               {
                   list[0] = lists;
                   key[0] = keys;
                   return;
               } 
           }

           i = k;
           j = k + k + 1;
           while (j <= ind)
           {
               if (j < ind)
               {
                   if (list[j] > list[j+1])  j = j + 1;
               }
               if (lists > list[j])
               {
                   list[i] = list[j];
                   key[i] = key[j];
                   i = j;
                   j = j + j + 1;
               } 
               else j = ind + 1;
           }

           list[i] = lists;
           key[i] = keys;

       } /* end while n > 1 */

   }    /* end descending order */

   else /* use ascending  order */
   {
       k = n/2;
       ind = n - 1;
       while (n > 1) 
       {
           if (k > 0)
           {
               k = k - 1;
               lists = list[k];
               keys = key[k];
           }
           else 
           {
               lists = list[ind];
               keys = key[ind];
               list[ind] = list[0];
               key[ind] = key[0];
               ind = ind - 1;
               if (ind <= 0)
               {
                   list[0] = lists;
                   key[0] = keys;
                   return;
               } 
           }

           i = k;
           j = k + k + 1;
           while (j <= ind)
           {
               if (j < ind)
               {
                   if (list[j] < list[j+1])  j = j + 1;
               }
               if (lists < list[j])
               {
                   list[i] = list[j];
                   key[i] = key[j];
                   i = j;
                   j = j + j + 1;
               } 
               else j = ind + 1;
           }

           list[i] = lists;
           key[i] = keys;

       } /* end while n > 1 */

   } /* end decending order */

   return;

} /* end sortx */
