package com.luma.wadloader3.ddd2application.controller;

import com.luma.wadloader3.ddd3domain.files.services.CmdTemplateManipulator;
import com.luma.wadloader3.ddd3domain.files.services.FileToZipFromWadService;
import com.luma.wadloader3.ddd3domain.files.services.FileZipper;
import com.luma.wadloader3.ddd3domain.files.services.TemplateArgs;
import com.luma.wadloader3.ddd3domain.wad.model.WadPack;
import com.luma.wadloader3.ddd3domain.wad.repos.WadPackRepo;
import com.luma.wadloader3.ddd3domain.wad.repos.WadRepo;
import com.luma.wadloader3.ddd4abstraction.functional.Failable;
import com.luma.wadloader3api.api.DownloadApi;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;

@RestController
@RequiredArgsConstructor
public class DownloadController implements DownloadApi {

    private final FileZipper fileZipper;
    private final WadRepo wadRepo;
    private final WadPackRepo wadPackRepo;
    private final FileToZipFromWadService fileToZipService;
    private final CmdTemplateManipulator<TemplateArgs> cmdTemplateManipulator;

    @Override
    public ResponseEntity<Resource> downloadHandlerGet() {
        FileSystemResource resource =
                new FileSystemResource(Path.of("wadloader3/src/main/resources/local-client.exe").toAbsolutePath());
        return ResponseEntity
                .ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=\"local-client.exe\"")
                .body(resource);
    }

    @Override
    public ResponseEntity<Resource> downloadSetupGet() {
        FileSystemResource resource =
                new FileSystemResource(Path.of("wadloader3/src/main/resources/wadloader.reg.bat").toAbsolutePath());
        return ResponseEntity
                .ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=\"wadloader.reg.bat\"")
                .body(resource);
    }

    //you can use "/download/wad/2,3,4" to download the wads with id 2,3,4
    @Override
    public ResponseEntity<Resource> downloadWadIdGet(List<Integer> ids) {
        return downloadWads(ids);
    }

    @Override
    public ResponseEntity<Resource> downloadWadPackNoWads(Integer id) {
        return downloadWadpackIdIdsGet(id, List.of());
    }

    @Override
    public ResponseEntity<Resource> downloadWadpackIdGet(Integer id) {
        Failable<WadPack> wadPackF = Failable.fromOptional(wadPackRepo.findById(id), "WadPack with id '%s' not found".formatted(id));

        //list of all wads to download
        Failable<List<FileZipper.FileToZip>> wadsF =
                wadPackF.map(wadPack -> wadPack.getWads()
                        .values()
                        .stream()
                        .map(fileToZipService::fromWad)
                        .toList());

        return downloadWadPack(id, wadPackF, wadsF);
    }

    @Override
    public ResponseEntity<Resource> downloadWadpackIdIdsGet(Integer wadPackId, List<Integer> wadIds) {
        Failable<WadPack> wadPackF = Failable.fromOptional(wadPackRepo.findById(wadPackId), "WadPack with id '%s' not found".formatted(wadPackId));

        //list of all wads to download
        Failable<List<FileZipper.FileToZip>> wadsF =
                wadPackF.map(wadPack -> wadPack.getWads()
                        .values()
                        .stream()
                        .filter(wad -> wadIds.contains(wad.getId()))
                        .map(fileToZipService::fromWad)
                        .toList());

        return downloadWadPack(wadPackId, wadPackF, wadsF);
    }

    private ResponseEntity<Resource> downloadWads(List<Integer> ids) {
        List<FileZipper.FileToZip> files = wadRepo.findAllById(ids).stream().map(fileToZipService::fromWad).toList();
        return fromFailableFilesToZip(Failable.success(files));
    }

    private ResponseEntity<Resource> downloadWadPack(Integer wadPackId, Failable<WadPack> wadPackF, Failable<List<FileZipper.FileToZip>> wadsF) {
        Failable<FileZipper.FileToZip> startScriptF = wadPackF.apply(wadPack -> cmdTemplateManipulator.fillTemplate(new TemplateArgs(wadPack.getWads(), String.valueOf(wadPackId)))).map(path -> new FileZipper.FileToZip(path, path.getFileName().toString()));


        Failable<List<FileZipper.FileToZip>> filesToZipF = wadsF.combine(startScriptF, (wadFiles, scriptFile) -> Stream.concat(Stream.of(scriptFile), wadFiles.stream()).toList());

        return fromFailableFilesToZip(filesToZipF);
    }

    private ResponseEntity<Resource> fromFailableFilesToZip(Failable<List<FileZipper.FileToZip>> files) {
        return switch (files.apply(fileZipper::zipFiles)) {
            case Failable.Success(Path zipPath) -> ResponseEntity.ok(new FileSystemResource(zipPath));
            case Failable.Failure<Path> failure -> {
                System.err.println(failure.error());
                yield ResponseEntity.badRequest().build();
            }
        };
    }
}
