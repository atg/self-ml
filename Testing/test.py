#!/usr/bin/python

# We run three tests on each testcase

# Image test:
#  1. Feed file to self-ml.
#  2. Feed output of self-ml to self-ml again.
#  3. Compare first and second outputs of self-ml. They should be equal.

# Output test:
#  1. Feed outputfile to self-ml.
#  2. Check that the output of self-ml is equal to the equivalent outputfile.

# Output image test:
#  1. Feed outputfile to self-ml
#  2. Feed output of self-ml to self-ml again
#  3. Compare first and second output. They should be equal.

import os, os.path
import subprocess

selfml_path = "../Implementations/C/Projects/Xcode/build/debug/self-ml"

def apply_selfml_file(path):
    p = subprocess.Popen([selfml_path, path], stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    p.wait()
    return p.stdout.read()

def apply_selfml_string(string):
    p = subprocess.Popen([selfml_path, "-c", string], stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    p.wait()
    return p.stdout.read()

for root, _, paths in os.walk("Testcases"):
    for path in paths:
        print("=== Testing " + path + " ===")
        testcase_path = os.path.join(root, path)
        output_path = os.path.join("Outputs", "output." + path)
        
        error = False
        
        try:
            # Image test
            t_image = apply_selfml_file(testcase_path)
            t_image2 = apply_selfml_string(t_image)
            
            if t_image != t_image2:
                print("\t" + path + " failed image test")
                error = True
            
            # Output test
            outputfile = open(output_path)
            o = outputfile.read()
            outputfile.close()
            
            o_image = apply_selfml_file(output_path)
                        
            if t_image != o:
                print("\t" + path + " failed output test")
                error = True
            
            # Output image test
            if t_image != o_image:
                print("\t" + path + " failed output image test")
                error = True
            
            # Output double image test
            o_image2 = apply_selfml_string(o_image)
            
            if t_image != o_image2:
                print("\t" + path + " failed output double image test")
                error = True
            
            if error == False:
                print("\tNO ERRORS")
        except:
            print("\tError with output file for " + path)
        
        print("")
        
