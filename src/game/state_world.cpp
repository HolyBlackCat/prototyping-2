#include "main.h"

#include "entities.h"
#include "gameutils/tiled_map.h"

static constexpr int tile_size = 12;

struct Map : Renderable
{
    IMP_STANDALONE_COMPONENT(Game)

    Array2D<int, int> tiles;
    Tiled::PointLayer points;

    void Load(Stream::Input source)
    {
        Json json(source.ReadToMemory().string(), 32);

        auto source_tiles = Tiled::LoadTileLayer(Tiled::FindLayer(json.GetView(), "mid"));
        tiles.resize(ivec2(source_tiles.size()));
        for (ivec2 pos : vector_range(tiles.size()))
            tiles.safe_nonthrowing_at(pos) = source_tiles.safe_nonthrowing_at(pos);

        points = Tiled::LoadPointLayer(Tiled::FindLayer(json.GetView(), "points"));
    }

    void Render() const override
    {
        auto cam_pos = game.get<Camera>()->pos;

        ivec2 corner_a = tiles.bounds().clamp(div_ex(cam_pos - screen_size/2, tile_size));
        ivec2 corner_b = tiles.bounds().clamp(div_ex(cam_pos + screen_size/2, tile_size));

        for (ivec2 tile_pos : corner_a <= vector_range <= corner_b)
        {
            int tile_id = tiles.safe_nonthrowing_at(tile_pos);
            if (tile_id == 0)
                continue;

            ivec2 pixel_pos = tile_pos * tile_size - cam_pos;

            r.iquad(pixel_pos, "tiles"_image with(().a.rect_size(tile_size)));
        }
    }
};

struct MouseCamera : Camera, Tickable
{
    void Tick() override
    {
        pos = mouse.pos();
    }
};

struct CenteredCamera : Camera, Tickable
{
    void Tick() override
    {
        if (auto map = game.get<Map>().get_opt())
            pos = (map->tiles.size() * tile_size) / 2;
    }
};

struct PhysicalObject
{
    IMP_COMPONENT(Game)

    ivec2 pos;
    fvec2 vel, vel_lag;
    bool ground = false;

    [[nodiscard]] virtual ivec2 HitboxSize() const = 0;

    [[nodiscard]] bool SolidAtOffset(Map &map, ivec2 offset) const
    {
        ivec2 size = HitboxSize();
        ivec2 a = -size / 2;
        ivec2 b = a + size - 1;
        return Math::for_each_cuboid_point(a, b, ivec2(tile_size), nullptr, [&](ivec2 point)
        {
            ivec2 tile = div_ex(pos + offset + point, tile_size);
            if (!map.tiles.bounds().contains(tile))
                return false;
            return map.tiles.safe_nonthrowing_at(tile) != 0;
        });
    }

    void PhysicsStep()
    {
        ivec2 desired_offset = Math::round_with_compensation(vel, vel_lag);

        auto &map = game.get<Map>().get();

        while (desired_offset)
        {
            for (int i = 0; i < 2; i++)
            {
                if (desired_offset[i] == 0)
                    continue;

                ivec2 step = ivec2::axis(i, sign(desired_offset[i]));

                if (SolidAtOffset(map, step))
                {
                    if (vel[i] * sign(desired_offset[i]) > 0)
                    {
                        vel[i] = 0;
                        if (vel_lag[i] * sign(desired_offset[i]) > 0)
                            vel_lag[i] = 0;
                    }
                    desired_offset[i] = 0;
                }
                else
                {
                    pos += step;
                    desired_offset[i] -= sign(desired_offset[i]);
                }
            }
        }

        vel_lag *= 0.99f;

        ground = SolidAtOffset(map, ivec2(0,1));
    }
};
using PhysicalObjects = Game::Category<Ent::OrderedList, PhysicalObject>;

struct Ship : Tickable, Renderable, PhysicalObject
{
    IMP_STANDALONE_COMPONENT(Game)

    bool controllable = false;
    bool facing_left = false;

    std::vector<Game::Id> passengers;

    ivec2 HitboxSize() const override
    {
        return ivec2(54, 32);
    }

    void Tick() override
    {
        ivec2 control;

        if (controllable)
            control = ivec2(Input::Button(Input::right).down() - Input::Button(Input::left).down(), Input::Button(Input::down).down() - Input::Button(Input::up).down());
        if (control.x)
            facing_left = control.x < 0;

        for (int i = 0; i < 2; i++)
        {
            if (control[i])
                clamp_var_abs(vel[i] += control[i] * 0.3f, 1);
            else
                vel[i] *= 0.95f;
        }

        if (!control)
            vel.y += 0.02f;

        PhysicsStep();
    }

    void Render() const override
    {
        ivec2 cam_pos = game.get<Camera>()->pos;

        r.iquad(pos - cam_pos, "ship"_image).center().flip_x(facing_left);
    }
};

struct Player : Tickable, Renderable, PhysicalObject
{
    bool facing_left = false;

    Game::Id ship_id;

    ivec2 HitboxSize() const override
    {
        return ivec2(10, 20);
    }

    void Tick() override
    {
        if (ship_id.is_nonzero())
        {
            if (Input::Button(Input::space).pressed())
            {
                auto &ship = *game.get<Ship>();
                pos = ship.pos;
                vel = fvec2{};
                vel_lag = fvec2{};
                Unboard();
            }
        }
        else
        {
            if (Input::Button(Input::space).pressed())
            {
                auto &ship = *game.get<Ship>();
                if (abs(ship.pos - pos) <= all(ship.HitboxSize()/2))
                    Board();
            }
        }

        if (ship_id.is_nonzero())
            return;

        int control = Input::Button(Input::right).down() - Input::Button(Input::left).down();

        if (control)
        {
            clamp_var_abs(vel.x += control * 0.3f, 2);
            facing_left = control < 0;
        }
        else
        {
            float dec = 0.3f;
            if (abs(vel.x) <= dec)
                vel.x = 0;
            else
                vel.x -= sign(vel.x) * dec;
        }

        if (ground && Input::Button(Input::up).pressed())
            vel.y = -3;

        if (ground && vel.y > 0)
            vel.y = 0;

        if (!ground)
            vel.y += 0.1f;

        PhysicsStep();
    }

    bool IsOnBoard() const
    {
        return ship_id.is_nonzero();
    }

    void Board()
    {
        auto &ship = *game.get<Ship>();
        ship_id = dynamic_cast<Game::Entity &>(ship).id();
        ship.controllable = true;
    }

    void Unboard()
    {
        game.get(ship_id).get<Ship>().controllable = false;
        ship_id = nullptr;
    }

    void Render() const override
    {
        if (ship_id.is_nonzero())
            return;

        ivec2 cam_pos = game.get<Camera>()->pos;
        r.iquad(pos - cam_pos, "player"_image with(().a.rect_size(24))).center().flip_x(facing_left);
    }
};



namespace States
{
    STRUCT( World EXTENDS StateBase )
    {
        MEMBERS()

        void Init() override
        {
            // Configure the audio.
            float audio_distance = screen_size.x * 3;
            Audio::ListenerPosition(fvec3(0, 0, -audio_distance));
            Audio::ListenerOrientation(fvec3(0,0,1), fvec3(0,-1,0));
            Audio::Source::DefaultRefDistance(audio_distance);

            // Entities.
            game = nullptr;

            game.create<CenteredCamera>();
            game.create<Map>().ref.Load(Program::ExeDir() + "assets/map.json");
            game.create<Ship>().ref.pos = game.get<Map>()->points.GetSinglePoint("ship").to<int>();
            game.create<Player>().ref.pos = game.get<Map>()->points.GetSinglePoint("player").to<int>();
        }

        void Tick(std::string &next_state) override
        {
            (void)next_state;

            for (auto &e : game.get<AllTickable>())
                e.get<Tickable>().Tick();
        }

        void Render() const override
        {
            Graphics::SetClearColor(fvec3(0));
            Graphics::Clear();

            r.iquad(ivec2().centered_rect_size(screen_size)).color(fvec3(0.172, 0.294, 0.607));

            r.BindShader();

            for (auto &e : game.get<AllRenderable>())
                e.get<Renderable>().Render();

            r.Finish();
        }
    };
}
