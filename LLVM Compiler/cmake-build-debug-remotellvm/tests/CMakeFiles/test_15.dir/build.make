# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.27

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /tmp/tmp.dORsRfq6MF

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm

# Include any dependencies generated for this target.
include tests/CMakeFiles/test_15.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include tests/CMakeFiles/test_15.dir/compiler_depend.make

# Include the progress variables for this target.
include tests/CMakeFiles/test_15.dir/progress.make

# Include the compile flags for this target's objects.
include tests/CMakeFiles/test_15.dir/flags.make

tests/test_15.bc.o: tests/test_15.bc
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --blue --bold --progress-dir=/tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating test_15.bc.o"
	cd /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/tests && clang-17 -c -o /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/tests/test_15.bc.o /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/tests/test_15.bc

tests/test_15.bc: p1
tests/test_15.bc: /tmp/tmp.dORsRfq6MF/tests/test_15.p1
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --blue --bold --progress-dir=/tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Generating test_15.bc"
	cd /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/tests && ../p1 /tmp/tmp.dORsRfq6MF/tests/test_15.p1 /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/tests/test_15.bc

tests/CMakeFiles/test_15.dir/test_15.c.o: tests/CMakeFiles/test_15.dir/flags.make
tests/CMakeFiles/test_15.dir/test_15.c.o: /tmp/tmp.dORsRfq6MF/tests/test_15.c
tests/CMakeFiles/test_15.dir/test_15.c.o: tests/CMakeFiles/test_15.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --progress-dir=/tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building C object tests/CMakeFiles/test_15.dir/test_15.c.o"
	cd /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/tests && /usr/bin/clang-17 $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -MD -MT tests/CMakeFiles/test_15.dir/test_15.c.o -MF CMakeFiles/test_15.dir/test_15.c.o.d -o CMakeFiles/test_15.dir/test_15.c.o -c /tmp/tmp.dORsRfq6MF/tests/test_15.c

tests/CMakeFiles/test_15.dir/test_15.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Preprocessing C source to CMakeFiles/test_15.dir/test_15.c.i"
	cd /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/tests && /usr/bin/clang-17 $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /tmp/tmp.dORsRfq6MF/tests/test_15.c > CMakeFiles/test_15.dir/test_15.c.i

tests/CMakeFiles/test_15.dir/test_15.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Compiling C source to assembly CMakeFiles/test_15.dir/test_15.c.s"
	cd /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/tests && /usr/bin/clang-17 $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /tmp/tmp.dORsRfq6MF/tests/test_15.c -o CMakeFiles/test_15.dir/test_15.c.s

# Object files for target test_15
test_15_OBJECTS = \
"CMakeFiles/test_15.dir/test_15.c.o"

# External object files for target test_15
test_15_EXTERNAL_OBJECTS = \
"/tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/tests/test_15.bc.o"

tests/test_15: tests/CMakeFiles/test_15.dir/test_15.c.o
tests/test_15: tests/test_15.bc.o
tests/test_15: tests/CMakeFiles/test_15.dir/build.make
tests/test_15: tests/CMakeFiles/test_15.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --bold --progress-dir=/tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Linking C executable test_15"
	cd /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/tests && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/test_15.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tests/CMakeFiles/test_15.dir/build: tests/test_15
.PHONY : tests/CMakeFiles/test_15.dir/build

tests/CMakeFiles/test_15.dir/clean:
	cd /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/tests && $(CMAKE_COMMAND) -P CMakeFiles/test_15.dir/cmake_clean.cmake
.PHONY : tests/CMakeFiles/test_15.dir/clean

tests/CMakeFiles/test_15.dir/depend: tests/test_15.bc
tests/CMakeFiles/test_15.dir/depend: tests/test_15.bc.o
	cd /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /tmp/tmp.dORsRfq6MF /tmp/tmp.dORsRfq6MF/tests /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/tests /tmp/tmp.dORsRfq6MF/cmake-build-debug-remotellvm/tests/CMakeFiles/test_15.dir/DependInfo.cmake "--color=$(COLOR)"
.PHONY : tests/CMakeFiles/test_15.dir/depend

