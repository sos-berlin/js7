package com.sos.jobscheduler.master.javaapi;

import com.sos.jobscheduler.base.problem.Problem;
import com.sos.jobscheduler.core.filebased.Repo;
import java.io.IOException;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import static java.io.File.separator;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.nio.file.Files.delete;
import static java.nio.file.Files.write;
import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;

/**
 * @author Joacim Zschimmer
 */
final class MasterRepoReaderTester {
    private final Path directory;
    private final MasterRepoReader repoReader;

    MasterRepoReaderTester(Path directory) {
        this.directory = directory;
        this.repoReader = new MasterRepoReader(directory).keepLastVersionOnly();
    }

    private static final String TestWorkflowJson =
      "{" +
        "\"instructions\": [" +
          "{ \"TYPE\": \"Job\", \"jobPath\": \"/JOB\", \"agentPath\": \"/AGENT\" }" +
        "]" +
      "}";

    void test() throws IOException {
        Repo repo = Repo.empty();  // repo holds the already read configuration objects

        write(directory.resolve("A.workflow.json"), TestWorkflowJson.getBytes(UTF_8));
        write(directory.resolve("B.workflow.json"), TestWorkflowJson.getBytes(UTF_8));
        repo = repoOrThrow(repoReader.applyConfigurationDirectory(repo, Optional.of("1")));

        Object result1 = repoReader.applyConfigurationDirectory(repo, Optional.of("1"));
        assert problemsToStrings(result1).equals(singletonList("Duplicate VersionId '1'"));

        write(directory.resolve("UNKNOWN.tmp"), "?".getBytes(UTF_8));
        write(directory.resolve("NO-JSON.workflow.json"), "INVALID JSON".getBytes(UTF_8));
        write(directory.resolve("ERROR-1.workflow.json"), "{ \"something\": \"different\" }".getBytes(UTF_8));
        write(directory.resolve("ERROR-2.workflow.json"), "{ \"instructions\": 999 }".getBytes(UTF_8));
        Object result2 = repoReader.applyConfigurationDirectory(repo);
        Set<String> errors = new HashSet<>(problemsToStrings(result2));
        Set<String> expected = new HashSet<>(asList(
            "Problem with 'Workflow:/ERROR-2' (JSON) [C[A]: DownField(instructions)]",
            "File '..."+ separator +"UNKNOWN.tmp' is not recognized as a configuration file",
            "Problem with 'Workflow:/NO-JSON' (JSON) [expected json value got I (line 1, column 1)]",
            "Problem with 'Workflow:/ERROR-1' (JSON) [Attempt to decode value on failed cursor: DownField(instructions)]"));
        assert errors.equals(expected) : errors + " did not equal " + expected;

        delete(directory.resolve("NO-JSON.workflow.json"));
        delete(directory.resolve("UNKNOWN.tmp"));
        delete(directory.resolve("ERROR-1.workflow.json"));
        delete(directory.resolve("ERROR-2.workflow.json"));
        repo = repoOrThrow(repoReader.applyConfigurationDirectory(repo));

        delete(directory.resolve("B.workflow.json"));
        write(directory.resolve("C.workflow.json"), TestWorkflowJson.getBytes(UTF_8));
        write(directory.resolve("D.workflow.json"), TestWorkflowJson.getBytes(UTF_8));
        repo = repoOrThrow(repoReader.applyConfigurationDirectory(repo));
        assert repo.idToFileBased().size() == 3 : "repo.idToFileBased.size = " + repo.idToFileBased().size();
        assert repo.versions().size() == 1 : "repo.versions.size = " + repo.versions().size();
    }

    private static Repo repoOrThrow(Object result) {
        if (result instanceof List<?>) {
            @SuppressWarnings("unchecked")
            List<Problem> problems = (List<Problem>)result;
            throw new RuntimeException("Master configuration directory has errors:\n" +
                problems.stream().map(Problem::toString).reduce((a, b) -> a + "\n" + b).orElse(""));
        }
        return (Repo)result;
    }

    private static List<String> problemsToStrings(Object problems) {
        @SuppressWarnings("unchecked")
        List<Problem> problems_ = (List<Problem>)problems;
        return asList(problems_.stream().map(Problem::toString).toArray(String[]::new));
    }
}
