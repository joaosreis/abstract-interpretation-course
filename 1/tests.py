#!/usr/bin/python3
# Cours "Semantics and applications to verification"
#
# Marc Chevalier 2018
# Ecole normale supérieure, Paris, France / CNRS / INRIA
import json
import argparse
import subprocess

# This may be to edit, but they are settable via command line
cmd = "./TP1.byte"

"""
# Suite format
There are 3 kind of basic objects: "folder", "file" and "suite"

## Common
"kind", string, can take the values listed above. Required.
"name", string, can be anything (legal in a path). Required.
"deterministic", bool. Optional.
If running only deterministic test cases, exploring will stop when the first
"deterministic": false, is encountered. "deterministic" value is inherited
across "file" and "folder" (not "suite").
cf. "file" section for more.

## Folder
"content", list of basic objects. The content of the directories. Can be other
subdirectories, files (test cases) or other test_suite files.

## Suite
Specify a file in the same JSON format that described other test suites.

## File
"terminate": bool. Whether the test should terminate or not. Required.
"fatal": bool. Whether a fail is a fatal error or not. It is totally
subjective. The intuitive meaning is that a fatal error should never happen,
but a non fatal error can, depending on the implementation. But fail is usually
not a good news!
"exit": int. Expected returned value of the program. Required if "terminate" is
true. Unused otherwise.
"timeout": int/float. The time before killing the process and deciding it does
not terminate. Required when "terminate" is false. Unused otherwise. The tester
wait for the process to terminate (which should not happen) or timeout. Don't
choose a too high timeout!
"stdout":
    if "deterministic" is true:
        list of list of {"var": var, "val": val}.
        Each list represent a print statement.
        Each sublist is the printing of a variable.
        Each object is the state of the variable.
    if "deterministic" is false:
        list of {"vars": vars, "vals": vals}
        where vars is a list of string (printed variables)
        and vals a list of lists of values: each sublist must be the same
        length as vars and reprent the value of each variable in an
        environment. vals is the list of all possibles values.
        Sensitive to the order right now, use is discouraged.
    Optional. Unused if "terminate" is false.
    The printing is understood as a deterministic printing if "deterministic"
    is true and the tester is run with "--deterministic".
"""

test_cases = 0
success = 0
fails = 0
fatals = 0
tests = ""
all_tests = ""
BOLD = "\033[1m"
GRAY = "\033[30m"
RED = "\033[31m"
GREEN = "\033[32m"
YELLOW = "\033[33m"
BLUE = "\033[34m"
PURPLE = "\033[35m"
CYAN = "\033[36m"
RESET = "\033[0m"
OK = "{}OK{}".format(GREEN, RESET)
FAIL = "{}W{}".format(YELLOW, RESET)
FATAL = "{}E{}".format(RED, RESET)
GREEN_DOT = "{}.{}".format(GREEN, RESET)


class DictDiffer(object):
    """
    Calculate the difference between two dictionaries as:
    (1) items added
    (2) items removed
    (3) keys same in both but changed values
    (4) keys same in both and unchanged values
    """
    def __init__(self, actual_dict, expected_dict):
        self.current_dict, self.past_dict = actual_dict, expected_dict
        self.set_current, self.set_past = set(actual_dict.keys()), set(expected_dict.keys())
        self.intersect = self.set_current.intersection(self.set_past)

    def added(self):
        return self.set_current - self.intersect

    def missing(self):
        return self.set_past - self.intersect

    def changed(self):
        return set(o for o in self.intersect if self.past_dict[o] != self.current_dict[o])

    def correct(self):
        return set(o for o in self.intersect if self.past_dict[o] == self.current_dict[o])


class SetDiffer(object):
    """
    Calculate the difference between two sets as:
    (1) items added
    (2) items removed
    (3) keys same in both
    """
    def __init__(self, actual_dict, expected_dict):
        self.set_current, self.set_past = actual_dict, expected_dict
        self.intersect = self.set_current.intersection(self.set_past)

    def added(self):
        return self.set_current - self.intersect

    def missing(self):
        return self.set_past - self.intersect

    def correct(self):
        return self.intersect


def explore_test_suite(path, test_node, deterministic):
    global test_cases, success, fails, fatals, tests, test_suite_file_names
    kind = test_node["kind"]
    name = test_node["name"]
    deterministic = test_node.get("deterministic", deterministic)
    if path is None:
        folder = "."
        path = name
    else:
        folder = path
        path = "{}/{}".format(path, name)
    if deterministic is not None and not deterministic and only_deterministic:
        if kind == "folder":
            print("{}{}skiping non deterministic folder{} {}"
                  .format(BOLD, GRAY, RESET, path))
        elif kind == "file":
            print("{}{}skiping non deterministic test case{} {}"
                  .format(BOLD, GRAY, RESET, path))
        elif kind == "suite":
            print("{}{}skiping non deterministic suite{} {}"
                  .format(BOLD, GRAY, RESET, path))
        return
    if kind == "folder":
        print("{}{}exploring{} {}".format(BOLD, BLUE, RESET, path))
        content = test_node["content"]
        for c in content:
            explore_test_suite(path, c, deterministic)
    elif kind == "file":
        print("{}{}running{}   {}: ".format(BOLD, PURPLE, RESET, path), end='')
        terminate = test_node["terminate"]
        fatal = test_node["fatal"]
        if fatal:
            fail_msg = FATAL
        else:
            fail_msg = FAIL
        test_cases += 1
        if terminate:
            exit_code = test_node["exit"]
            stdout = test_node.get("stdout")
            p = subprocess.run([cmd, path], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            failed = False
            if (p.returncode != exit_code and not no_enforce_return_code) or \
                    (p.returncode == 0 and exit_code != 0 or
                     p.returncode != 0 and exit_code == 0):
                if not failed:
                    print("{} ".format(fail_msg), end='')
                    failed = True
                print("return code: got {} but expected {}. "
                      .format(p.returncode, exit_code), end="")
            if stdout is not None:
                try:
                    actual_stdout = [json.loads(line) for line in p.stdout.decode("utf-8")
                                     .strip("\n").split('\n') if line.strip()]
                except json.JSONDecodeError as e:
                    print("{} Got {} which is not JSON valid: {}"
                          .format(fail_msg, p.stdout.decode("utf-8").__repr__(), e.__str__()))
                    if fatal:
                        tests += FATAL
                        fatals += 1
                    else:
                        tests += FAIL
                        fails += 1
                    return
                expected_stdout = stdout
                for act, exp in zip(actual_stdout, expected_stdout):
                    if deterministic:
                        diff = DictDiffer(act, exp)
                        if check_stdout and not (len(diff.changed()) == len(diff.added()) == len(diff.missing()) == 0):
                            if not failed:
                                print("{} ".format(fail_msg), end='')
                                failed = True
                            print("Incorrect output: expected {} but got {}".format(json.dumps(exp), json.dumps(act)))
                    else:
                        diff = SetDiffer(set((tuple(sorted(e.items())) for e in act)),
                                         set((tuple(sorted(e.items())) for e in exp)))
                        if check_stdout and not (len(diff.added()) == len(diff.missing()) == 0):
                            if not failed:
                                print("{} ".format(fail_msg), end='')
                                failed = True
                            print("Incorrect output: expected {} but got {}".format(json.dumps(exp), json.dumps(act)))
            if failed:
                if fatal:
                    tests += FATAL
                    fatals += 1
                else:
                    tests += FAIL
                    fails += 1
                print("")
            else:
                tests += GREEN_DOT
                success += 1
                print(OK)
        else:
            timeout = test_node["timeout"]
            try:
                p = subprocess.run([cmd, path], timeout=timeout, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            except subprocess.TimeoutExpired:
                tests += GREEN_DOT
                success += 1
                print(OK)
            else:
                if fatal:
                    tests += FATAL
                    fatals += 1
                else:
                    tests += FAIL
                    fails += 1
                print("{} : terminate with return code {} "
                      "but timeout was expected".format(fail_msg, p.returncode))
    elif kind == "suite":
        print("{}{}discovering suite{} {}".format(BOLD, BLUE, RESET, path))
        test_suite_file_names.append((path, folder))
    else:
        print("Unknown kind {}".format(kind))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Basic tester for TP1')
    parser.add_argument('--cmd', dest='cmd', action='store',
                        type=str, default="./TP1.byte",
                        help='Set the command to test (default: ./TP1.byte)')
    parser.add_argument('test_suite', nargs='*',
                        help='Test suites to run. If none is provided, use'
                        'test_suite.json')
    parser.add_argument('--no-check-stdout', action='store_true',
                        help="Don't compore outputs", dest='no_check_stdout')
    parser.add_argument('--no-enforce-return-code', action='store_true',
                        help="Just compare if both return code are 0 or !=0",
                        dest='no_enforce_return_code')
    parser.add_argument('--deterministic', action='store_true',
                        help="Only run deterministic test cases. Compare"
                        "output using simple pattern. Default behaviour: run"
                        "everything, use non deterministic pattern.",
                        dest='deterministic')
    parser.add_argument('--no-color', action='store_true',
                        help="Disable color in output. If you have some retarded terminal...",
                        dest='no_color')
    args = parser.parse_args()

    if args.no_color:
        BOLD = ""
        GRAY = ""
        RED = ""
        GREEN = ""
        YELLOW = ""
        BLUE = ""
        PURPLE = ""
        CYAN = ""
        RESET = ""

    cmd = args.cmd
    test_suite_file_names = args.test_suite
    check_stdout = not args.no_check_stdout
    no_enforce_return_code = args.no_enforce_return_code
    only_deterministic = args.deterministic
    total_test_cases = 0
    total_success = 0
    total_fails = 0
    total_fatals = 0

    if len(test_suite_file_names) == 0:
        test_suite_file_names = [("test_suite.json", None)]

    while len(test_suite_file_names) > 0:
        test_suite_fle_name, base_path = test_suite_file_names.pop()
        with open(test_suite_fle_name, "r") as test_suite_file:
            test_suite = json.load(test_suite_file)

        print("{}{}Running test suite:{} {}".format(CYAN, BOLD, RESET,
                                                    test_suite_fle_name))
        explore_test_suite(base_path, test_suite, None)
        print("Tests: {}; Passed: {}{}{}; Failed: {}{}{}; Fatals: {}{}{}".
              format(test_cases,
                     GREEN + BOLD if success != 0 else "",
                     success,
                     RESET if success != 0 else "",
                     YELLOW + BOLD if fails != 0 else "",
                     fails,
                     RESET if fails != 0 else "",
                     RED + BOLD if fatals != 0 else "",
                     fatals,
                     RESET if fatals != 0 else ""
                     )
              )
        if tests != "":
            print(tests)
        if success == test_cases:
            print("{}\n".format(OK))
        elif fatals == 0:
            print("{}\n".format(FAIL))
        else:
            print("{}\n".format(FATAL))
        total_test_cases += test_cases
        test_cases = 0
        total_success += success
        success = 0
        total_fails += fails
        fails = 0
        total_fatals += fatals
        fatals = 0
        if tests != "":
            if all_tests != "":
                all_tests += "|"
            all_tests += tests
            tests = ""

    print("{}{}Summary:{}".format(CYAN, BOLD, RESET))
    print("Tests: {}; Passed: {}{}{}; Failed: {}{}{}; Fatals: {}{}{}".
          format(total_test_cases,
                 GREEN + BOLD if total_success != 0 else "",
                 total_success,
                 RESET if total_success != 0 else "",
                 YELLOW + BOLD if total_fails != 0 else "",
                 total_fails,
                 RESET if total_fails != 0 else "",
                 RED + BOLD if total_fatals != 0 else "",
                 total_fatals, RESET if total_fatals != 0 else ""
                 )
          )
    print(all_tests)
    if total_success == total_test_cases:
        print("{}".format(OK))
    elif total_fatals == 0:
        print("{}".format(FAIL))
    else:
        print("{}".format(FATAL))
