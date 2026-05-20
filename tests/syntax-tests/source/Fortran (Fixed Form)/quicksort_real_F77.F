C  Fortran 77 implementation of a quicksort algorithm for arrays with
C  real entries.
C  ----------
C  June 2019 
C  Jason Allen Anema, Ph.D.
C  Division of Statistical Genomics
C  Department of Genetics
C  Washington University School of Medicine in St. Louis
C 
C  This work is partially supported NIH grant AG023746
C  ----------
C  Insertion sort is used for short arrays, as quicksort is slower on
C  these.
C  
C  Hoare partition scheme is used (sweeping left and right), as it does
C  three times fewer swaps on average that the Lamuto partition scheme.
C  In conjunction with this, tripartite partition is performed
C  concurrently (solving the "Dutch National Flag problem"). This avoids 
C  horrible runtimes on highly repetitive arrays. For example, without  
C  this, an array of random zeros and ones would have a runtime of
C  O(N^2), but now has a runtime of O(N). The runtime for this algorthm
C  on arrays with k highly repetitive entries is now O(kN).
C    
C  For medium length (sub)arrays, pivots are choosen using
C  Median-of-Three, and those three items are sorted. For longer (sub)arrays
C  the pseudomedian of nine (Median of medians). This avoids O(N^2) runtime on
C  nonrandom inputs such as increasing and decreasing sequences. 
C
C  See Louis Bentley, Jon & McIlroy, Douglas. (1993). Engineering a Sort Function.
C  Softw., Pract. Exper.. 23. 1249-1265. 10.1002/spe.4380231105 for details. 
C
C  The ordering on elements of the array are defined by a comparison
C  function,compar, that is a user-supplied INTEGER*2 function of the form
C           compar(a,b) which returns:
C              -1 if a precedes b
C              +1 if b precedes a
C              0 is a and b are considered equivalent
C  and thus defines a total ordering.
C  
C  If one would like to use the standard order on integers, the
C  compar function could be written in a file "compint.F" as:
C  ----------------------------------------------------------------
C      INTEGER*2 FUNCTION compint(a,b)
C      INTEGER a, b
C      if(a.lt.b)then
C         compint = -1
C      elseif(a.gt.b)then
C         compint = +1
C      else
C         compint = 0
C      endif
C      END
C  ----------------------------------------------------------------
C  Then in your program, call quicksort with:
C      call quicksort_real_F77(array, n, compint)
C
C  The maximal length of an array in this implementation is (2^31-1),
C  but can be changed to allow for length up to (2^63-1) by changing the
C  data types of the relevant variables and constants. If you wish to 
C  sort longer arrays, of length N, you'll need to customize variable 
C  and constant types and set mstack to be at least (2*log_2(N)+2). 
C
C  ----------------------------------------------------------------
C  Copyright 2019 Jason Allen Anema
C  
C  Permission is hereby granted, free of charge, to any person obtaining
C  a copy of this software and associated documentation files (the "Software"),
C  to deal in the Software without restriction, including without limitation the
C  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
C  sell copies of the Software, and to permit persons to whom the Software is
C  furnished to do so, subject to the following conditions:
C
C  The above copyright notice and this permission notice shall be included
C  in all copies or substantial portions of the Software.
C
C  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
C  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
C  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
C  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
C  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
C  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
C  IN THE SOFTWARE.
C  -------------------------------------------------------------------
C
      SUBROUTINE quicksort_real_F77(array,n,compar)
      INTEGER n, maxins, maxmid, mstack
      REAL array(n)
      PARAMETER (maxins = 7, maxmid = 40, mstack = 128)
C        maxins: maximal size of (sub)arrays to be sorted with
C           insertion sort.
C        maxmid: maximal size of (sub)arrays that will be quicksorted with
C           Median-of-Three pivots.
C        mstack: maximal size of required auxiliary storage (a stack), plus 2 
C           extra spots, which tracks the starts and ends of yet unsorted 
C           subarrays. mstack = 130 is large enough to handle arrays up to 
C           length 2^63-1. This maximal size follows from
C           processing smaller arrays first and pigeonhole principal.
C 
      INTEGER  a, d, i, j, k, s, lo, mid, hi, tstack, bstack(mstack)
C        a, d, i, j, k, s: indices
C        lo, mid, and hi: their natural location in a (sub)array
C        tstack:  equal to twice the number of additional subarrays still 
C          needing to be sorted
C        bstack: stack of the endpoints of unsorted subarrays
      INTEGER pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9
C        for pseudomedian of nine positions in (sub)arrays
      REAL piv, temp
C        piv is to store the pivot's value
C    
      EXTERNAL compar
      INTEGER*2 compar
C        compar is a user-supplied INTEGER*2 function of the form
C           compar(a,b) which returns:
C              -1 if a precedes b
C              +1 if b precedes a
C              0 is a and b are considered equivalent
C           and thus defines a total ordering. 
      tstack = 0
      lo = 1 
      hi = n
C
C  Insertion sort subarrays of size maxins or less
 1    if(hi-lo+1.le.maxins)then
         do 10, i = lo + 1, hi, 1
            temp = array(i)
            do 11 j = i - 1, lo, -1
               if(compar(array(j), temp).le.0)goto 2
               array(j+1)=array(j)
 11         continue
            j = lo - 1
 2          array(j+1) = temp
 10      continue
         if(tstack.eq.0)return
C  Pop the bstack, and start new partitioning
         hi = bstack(tstack)
         lo = bstack(tstack-1)
         tstack = tstack - 2
      else
C  Use Median-of-Three as choice of pivot (median of lo, middle, hi)
C  and reorder those elements appropriately when subarrays are medium
C  length (between maxins and maxmid)
         mid = lo +  (hi-lo)/2
         if(hi-lo.le.maxmid)then
            if(compar(array(mid), array(lo)).eq.-1)then
               temp = array(lo)
               array(lo) = array(mid)
               array(mid) = temp
            endif
            if(compar(array(hi), array(lo)).eq.-1)then
               temp = array(hi)
               array(hi) = array(lo)
               array(lo) = temp
            endif
            if(compar(array(hi), array(mid)).eq.-1)then
               temp = array(hi)
               array(hi) = array(mid)
               array(mid) = temp
            endif
C  Use pseudomedian of nine (Median of medians) as choice of pivot when 
C  subarrays are longer than maxmid. Note that doing it this way requires only 12
C  comparisons for finding the pivot.
         elseif(hi-lo+1.gt.maxmid)then
            pm1 = lo
            pm5 = lo + (hi-lo)/2
            pm9 = hi
            pm3 = lo + (pm5-lo)/2
            pm7 = pm5 + (hi-pm5)/2
            pm2 = lo + (pm3-lo)/2
            pm4 = pm3 + (pm5-pm3)/2
            pm6 = pm5 + (pm7-pm5)/2
            pm8 = pm7 + (pm9-pm7)/2
C  Median and sorting for pm1, pm2, pm3
            if(compar(array(pm2), array(pm1)).eq.-1)then
               temp = array(pm1)
               array(pm1) = array(pm2)
               array(pm2) = temp
            endif
            if(compar(array(pm3), array(pm1)).eq.-1)then
               temp = array(pm3)
               array(pm3) = array(pm1)
               array(pm1) = temp
            endif
            if(compar(array(pm3), array(pm2)).eq.-1)then
               temp = array(pm3)
               array(pm3) = array(pm2)
               array(pm2) = temp
            endif
C  Median and sorting for pm4, pm5, pm6
            if(compar(array(pm5), array(pm4)).eq.-1)then
               temp = array(pm4)
               array(pm4) = array(pm5)
               array(pm5) = temp
            endif
            if(compar(array(pm6), array(pm4)).eq.-1)then
               temp = array(pm6)
               array(pm6) = array(pm4)
               array(pm4) = temp
            endif
            if(compar(array(pm6), array(pm5)).eq.-1)then
               temp = array(pm6)
               array(pm6) = array(pm5)
               array(pm5) = temp
            endif
C  Median and sorting for pm7, pm8, pm9
            if(compar(array(pm8), array(pm7)).eq.-1)then
               temp = array(pm7)
               array(pm7) = array(pm8)
               array(pm8) = temp
            endif
            if(compar(array(pm9), array(pm7)).eq.-1)then
               temp = array(pm9)
               array(pm9) = array(pm7)
               array(pm7) = temp
            endif
            if(compar(array(pm9), array(pm8)).eq.-1)then
               temp = array(pm9)
               array(pm9) = array(pm8)
               array(pm8) = temp
            endif
C Median of the medians (which are now pm2, pm5, pm8)
            if(compar(array(pm5), array(pm2)).eq.-1)then
               temp = array(pm2)
               array(pm2) = array(pm5)
               array(pm5) = temp
            endif
            if(compar(array(pm8), array(pm2)).eq.-1)then
               temp = array(pm8)
               array(pm8) = array(pm2)
               array(pm2) = temp
            endif
            if(compar(array(pm8), array(pm5)).eq.-1)then
               temp = array(pm8)
               array(pm8) = array(pm5)
               array(pm5) = temp
            endif
         endif
C  Pivot assigned for medium and long length subarrays.
C  Note that pm5 = mid
             piv = array(mid)
C  Initialize pointers for partitioning
            i = lo-1
            j = hi+1
C  Initialize counts of repeat values of pivot.
            a = 0
            d = 0
C  Beginning of outer loop for placing pivot.
 3       continue
C  Scan up to find an element > piv.
            i = i + 1
C  Check if pointers crossed.
         if(j.lt.i)goto 5
C  Check if i pointer hit hi boundary.
         if(i.eq.hi)goto 4
C      
         if(compar(array(i), piv).eq.-1)goto 3
C Check for copies of pivot from scanning right. 
         if(compar(array(i), piv).eq.0)then
             array(i) = array(lo+a)
             array(lo+a) = piv
             a = a + 1
             goto 3 
         endif
C  Beginning of innerloop for placing pivot.
 4       continue
C  Scan down to find an element < piv.
            j = j - 1
C  Check if pointers crossed.
         if(j.lt.i)goto 5
         if(compar(array(j), piv).eq.1)goto 4
C  Check for copies of pivot from scanning left. 
         if(compar(array(j), piv).eq.0)then
            array(j) = array(hi-d)
            array(hi-d) = piv
            d = d + 1
            goto 4      
         endif
C Check if pointers crossed.
         if(j.lt.i)goto 5
C  Exchange elements
         temp = array(i)
         array(i) = array(j)
         array(j) = temp
C  End of outermost loop for placing pivot.
         goto 3
C  Insert all copies of pivot in appropriate place
 5       s = MIN(a, j-lo-a+1)
         DO 6 k = 1, s
            array(lo-1+k) = array(i-k)
            array(i-k) = piv
 6       CONTINUE
         s = MIN(d, hi-j-d)
         DO 7 k = 1, s
            array(hi+1-k) = array(j+k)
            array(j+k) = piv
 7       CONTINUE
C  Increase effective stack size
         tstack = tstack + 2 
C  Push pointers to larger subarray on stack for later processing,
C  process smaller subarray immediately. 
         if(tstack.gt.mstack) THEN
           WRITE(*,*)'Stack size is too small in quicksort fortran code quicksort_real_F77.F' 
           WRITE(*,*)'Are you sure you want to sort an array this long?'
           WRITE(*,*)'Your array has more than 2^63-1  entries?'
           WRITE(*,*)'If so, set mstack parameter to be at least:'
           WRITE(*,*)'2*ceiling(log_2(N))+2, for N = length of array,'
           WRITE(*,*)'and recompile this subroutine.'
           RETURN 
         endif
         if(hi-j-d-1.ge.j-a-lo)then
            bstack(tstack) = hi
            bstack(tstack-1) = MIN(j+d+1, hi)
            hi=MAX(j-a,lo)
         else
            bstack(tstack)=MAX(j-a,lo)
            bstack(tstack-1)=lo
            lo=MIN(j+d+1,hi)
         endif
C
C     end of outermost if statement 
      endif
      goto 1
C     END of subroutine quicksort
      END
