
/**
 * Copyright (C) 2016 Lampros Mouselimis
 *
 * @file sort_2dim_vecs.h
 *
 * @author Lampros Mouselimis
 *
 * @date October - December 2016
 *
 * @Notes: sorting of either vectors (2-dimensional) or of an unordered-map (2-dimensional values)
 *
 * @last_modified: December 2016
 *
 **/


#ifndef __sort2DimVecs__
#define __sort2DimVecs__



// struct for all data types
//

template<class T1, class T2>
struct STRUCT {

  T1 VAR1;

  T2 VAR2;
};



// sort using operator()
//  http://stackoverflow.com/questions/2155675/can-i-pass-a-parameter-to-an-stdvector-sort-function?rq=1
//

template<class T1, class T2>
class SORT_template {

  private:

    bool var1_sort;

    bool ascending;

  public:

    SORT_template(bool tmp_sort, bool asc) : var1_sort(tmp_sort), ascending(asc) {}


    // sort-function
    //

    bool STRUCT_SORT(const STRUCT<T1, T2> &x, const STRUCT<T1, T2> &y) {

      if (var1_sort) {

        if (ascending) {

          return x.VAR1 < y.VAR1;}

        else {

          return x.VAR1 > y.VAR1;
        }
      }

      else {

        if (ascending) {

          return x.VAR2 < y.VAR2;}

        else {

          return x.VAR2 > y.VAR2;
        }
      }
    }


    // sort-operator()
    //

    bool operator() (const STRUCT<T1, T2> &x, const STRUCT<T1, T2> &y) {

      return STRUCT_SORT(x, y);
    }


    ~SORT_template() { }
};




// sort 2-dimensional vector
//


template<class T1, class T2>
class SORT_2DIMENSIONAL_VEC {

  public:

    SORT_2DIMENSIONAL_VEC() { }


    // sort vectors of any data type using a struct
    //

    std::vector<STRUCT<T1, T2>> inner_sort_func_VEC(std::vector<T1> V1, std::vector<T2> V2, bool sort, bool ascend, int threads = 1) {

      #ifdef _OPENMP
      omp_set_num_threads(threads);
      #endif

      std::vector<STRUCT<T1, T2>> struct_result(V1.size());

      #ifdef _OPENMP
      #pragma omp parallel for schedule(static)
      #endif
      for (unsigned int i = 0; i < V1.size(); i++) {

        STRUCT<T1, T2> tmp_struct;

        tmp_struct.VAR1 = V1[i];

        tmp_struct.VAR2 = V2[i];

        struct_result[i] = tmp_struct;
      }

      SORT_template<T1,T2> srt(sort, ascend);                     // composition inside class-method

      std::sort(struct_result.begin(), struct_result.end(), srt);

      return struct_result;
    }



    // sort a map of any data type using a struct
    //

    std::vector<STRUCT<T1, T2>> inner_sort_func_MAP(std::unordered_map<T1, T2> my_map, bool sort, bool ascend) {

      std::vector<STRUCT<T1, T2>> struct_result;

      for (auto& it : my_map) {

        STRUCT<T1, T2> tmp_struct;

        tmp_struct.VAR1 = it.first;

        tmp_struct.VAR2 = it.second;

        struct_result.push_back(tmp_struct);
      }

      SORT_template<T1,T2> srt(sort, ascend);                     // composition inside class-method

      std::sort(struct_result.begin(), struct_result.end(), srt);

      return struct_result;
    }

    ~SORT_2DIMENSIONAL_VEC() { }          // destructor
};



#endif
