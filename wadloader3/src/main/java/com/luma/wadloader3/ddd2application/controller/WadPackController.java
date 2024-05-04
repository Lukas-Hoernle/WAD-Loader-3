package com.luma.wadloader3.ddd2application.controller;

import com.luma.wadloader3.ddd2application.controller.mappers.WadPackMapper;
import com.luma.wadloader3.ddd3domain.wad.model.Wad;
import com.luma.wadloader3.ddd3domain.wad.model.WadPack;
import com.luma.wadloader3.ddd3domain.wad.repos.WadPackRepo;
import com.luma.wadloader3.ddd3domain.wad.repos.WadRepo;
import com.luma.wadloader3.ddd4abstraction.functional.Failable;
import com.luma.wadloader3api.api.WadpackApi;
import com.luma.wadloader3api.model.NewWadPackDto;
import com.luma.wadloader3api.model.WadDto;
import com.luma.wadloader3api.model.WadPackDto;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.toMap;

@RestController
@RequiredArgsConstructor
public class WadPackController implements WadpackApi {

    private final WadPackRepo wadPackRepo;
    private final WadRepo wadRepo;
    private final WadPackMapper wadPackMapper;

    @Override
    public ResponseEntity<Void> wadpackIdDelete(Integer id) {
        wadPackRepo.deleteById(id);
        return ResponseEntity.status(200).build();
    }

    @Override
    public ResponseEntity<WadPackDto> wadpackIdGet(Integer id) {
        return wadPackRepo.findById(id)
                .map(wadPackMapper::map)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.badRequest().build());
    }

    @Override
    public ResponseEntity<WadPackDto> wadpackPost(@Valid NewWadPackDto newWadPackDto) {
        switch (toLoadOrder(newWadPackDto.getWads())) {
            case Failable.Success<Map<Integer, Wad>>(Map<Integer, Wad> wadOrder) -> {
                WadPack wadPack = WadPack.builder()
                        .name(newWadPackDto.getName())
                        .description(newWadPackDto.getDescription())
                        .wads(wadOrder)
                        .build();

                wadPack = wadPackRepo.save(wadPack);

                return ResponseEntity.ok(wadPackMapper.map(wadPack));
            }
            case Failable.Failure(List<String> error) -> {
                System.err.println(error);
                return ResponseEntity.badRequest().build();
            }
        }
    }

    @Override
    public ResponseEntity<WadPackDto> wadpackIdPut(Integer id, @Valid NewWadPackDto newWadPackDto) {

        Failable<WadPackDto> updatedFailable = Failable.fromOptional(wadPackRepo.findById(id), "no WadPack with id %d".formatted(id))
                .combine(toLoadOrder(newWadPackDto.getWads()), (oldWadPack, loadOrder) -> {
                    oldWadPack.setWads(loadOrder);
                    oldWadPack.setName(newWadPackDto.getName());
                    oldWadPack.setDescription(newWadPackDto.getDescription());
                    wadPackRepo.save(oldWadPack);
                    return wadPackMapper.map(oldWadPack);
                });

        switch (updatedFailable) {
            case Failable.Success<WadPackDto>(WadPackDto wadPackDto) -> {
                return ResponseEntity.ok(wadPackDto);
            }
            case Failable.Failure(List<String> error) -> {
                System.err.println(error);
                return ResponseEntity.badRequest().build();
            }
        }
    }

    private Failable<Map<Integer, Wad>> toLoadOrder(List<WadDto> wadDtos) {
        List<Integer> wadIds = wadDtos.stream().map(WadDto::getId).toList();
        List<Wad> wads = wadRepo.findAllById(wadIds);


        if (wads.size() < wadIds.size()) {
            return Failable.Failure.of("Not all WadIds found. expected: %s but found %s".formatted(
                    wadIds,
                    wads.stream().map(Wad::getId).toList()));
        }

        return wads.stream()
                .collect(collectingAndThen(
                        //turn into map and wrap into Failable.Success
                        toMap(wad -> wadIds.indexOf(wad.getId()), Function.identity()),
                        Failable.Success::new));
    }
}
