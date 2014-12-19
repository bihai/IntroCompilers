import os

# This imports the global environment -- such as environment
# variables. Without this, the PATH to javac (/s/std/bin) would
# not be available.
env = Environment(ENV = os.environ)

# Abbreviate these so you can change them easier
source_dir = 'src'
build_dir = 'bin'

env['GEN_DIR'] = 'gen'
env['ANTLR_JAR'] = '/u/c/s/cs536-1/public/programs/antlr-3.4-complete.jar'

if 'CLASSPATH' not in os.environ:
   classpath = build_dir + ':' + env['ANTLR_JAR']
else:
   classpath = os.environ['CLASSPATH'] + ':' + build_dir + ':' + env['ANTLR_JAR']

env['JAVACLASSPATH'] = classpath
print env['JAVACLASSPATH']

##
## Generating the ANTLR grammar
##
env.Command('$GEN_DIR/src/cs536/syntax/MinC.java', 'src/cs536/syntax/MinC.g',
            'java -cp $ANTLR_JAR org.antlr.Tool -o $GEN_DIR $SOURCE')

env.Clean('$GEN_DIR/src/cs536/syntax/MinC.java', '$GEN_DIR')

##
## Building the .class files
##

# Compile all the .java files in src/ to bin/ (the SCons target
# specifications work like assignment in languages: the target is the first
# parameter)
classes = env.Java(build_dir, [source_dir, '$GEN_DIR/src/cs536/syntax/MinC.java'])

# If we run 'scons' with no arguments, we just want to build the result
Default(classes)

# Make "scons compile" do the right thing
env.Alias('compile', classes)
