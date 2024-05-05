package com.luma.wadloader3.ddd1infrastructure;

import com.luma.wadloader3.ddd1infrastructure.config.WadDir;
import com.luma.wadloader3.ddd3domain.files.services.CmdTemplateManipulator;
import com.luma.wadloader3.ddd3domain.files.services.FileToZipFromWadService;
import com.luma.wadloader3.ddd3domain.files.services.FileZipper;
import com.luma.wadloader3.ddd3domain.files.services.TemplateArgs;
import com.luma.wadloader3.ddd4abstraction.functional.Failable;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class CmdTemplateManipulatorImpl implements CmdTemplateManipulator<TemplateArgs> {

    private final static Path templatePath = Path.of("startWadTemplate.cmd");
    private final static String replaceVariable = "%FILES%";
    private final FileToZipFromWadService fileToZipService;
    private final WadDir wadDir;

    @Override
    public Failable<Path> fillTemplate(TemplateArgs templateValues) {
        String replacementValue = templateValues.wads().entrySet().stream()
                .sorted(Map.Entry.comparingByKey())
                .map(Map.Entry::getValue)
                //we need the names of the files in the .zip file
                //because we are writing the command to start them with
                .map(fileToZipService::fromWad)
                .map(FileZipper.FileToZip::name)
                .map(String::valueOf)
                .reduce("", (acc, val) -> acc + " " + val);

        Failable<String> cmdScriptLines = Failable.run(() -> Files.readAllLines(wadDir.scriptDirPath().resolve(templatePath).toAbsolutePath())
                .stream()
                .map(s -> s.replaceAll(replaceVariable, replacementValue))
                .collect(Collectors.joining("\n")));

        Path filePath = wadDir.scriptDirPath().resolve("start%s.cmd".formatted(templateValues.name()));

        return cmdScriptLines.apply(script -> Failable.run(() -> Files.writeString(filePath, script)));
    }
}
